#!/usr/bin/env python3
"""
Phase B - bounded inductive LLM pass over the 150-post sample (Study 1, exploratory).

Engine: the `claude` CLI (Claude Code, headless `-p`), which authenticates via the
Claude Code SUBSCRIPTION - no API key. This mirrors the June multimodal-coding run.

Three stages, model routing via the CLI `--model` flag:
  Stage 1  per-post open extraction   image + caption  -> claude-opus-4-8 (vision, reads the jpg)
  Stage 1b comment stance             caption + <=15 comments (posts with >=3) -> claude-opus-4-8
  Stage 2  taxonomy synthesis         all Stage-1 label sets (1 call)          -> claude-fable-5 (fallback opus)

Structured output: the prompt embeds the JSON Schema and asks for raw JSON only;
the result is parsed and cached. (The CLI has no forced-tool mode, so the schema
is described, not hard-enforced; parsing is defensive.)

Reproducibility / cost control:
  - Raw parsed object per post cached to output/llm_cache/<pid>_{stage1,stage1b}.json;
    a re-run reads the cache and spends nothing. Safe to interrupt and relaunch.
  - Per call, bundled skills are disabled to trim framework overhead.
  - NOTE: temperature is not settable via the subscription path and is irrelevant here;
    determinism rests on the fixed prompt + schema + per-post cache.
  - Cost is SUBSCRIPTION USAGE (each call ~tens of k tokens of harness overhead),
    not API dollars. The running total printed is the API-equivalent for reference.

Run with base python3 (only stdlib + pandas needed; pandas is in the NLP venv):
  "../4. NLP Pipeline/.venv/bin/python" b1_llm_taxonomy_pass.py --dry-run     # validate, no calls
  "../4. NLP Pipeline/.venv/bin/python" b1_llm_taxonomy_pass.py --limit 2     # smoke test (2 posts)
  "../4. NLP Pipeline/.venv/bin/python" b1_llm_taxonomy_pass.py               # full run (Stage 1 + 1b + 2)
  "../4. NLP Pipeline/.venv/bin/python" b1_llm_taxonomy_pass.py --stage 2     # re-synthesise from cache
"""
#
# PATHS IN THIS PACKAGE
# This script was written inside the thesis working tree, where its inputs sat in
# sibling folders named "2. Data Collection (Apify)" and the like. In the
# reproduction package the same files live under data/, and python/_paths.py
# holds the canonical locations.
#
# Status: RECORD ONLY. It cannot run here, because it needs a local language-model CLI, and its output is not deterministic.
# The outputs it produced are shipped and are the canonical record, so nothing in
# the reported results depends on rerunning it.
#
import argparse
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile

# --- input/output paths (all derived from this file's location) ---
BASE_DIR = os.path.dirname(os.path.abspath(__file__))
CA_DIR = os.path.abspath(os.path.join(BASE_DIR, ".."))   # the "1. Account & Content Analysis" root
OUT_DIR = os.path.join(BASE_DIR, "output")
CACHE_DIR = os.path.join(OUT_DIR, "llm_cache")           # one JSON per post+stage -> re-runs are free
MEDIA_DIR = os.path.join(CA_DIR, "3. Codebook & Coding", "media_test")   # the post jpgs the vision stage reads
CAPTION_CSV = os.path.join(CA_DIR, "4. NLP Pipeline", "NLP_Analysis",
                           "Study1_NLP_Framing_2026-06-10_0828.csv")
COMMENTS_JSON = os.path.join(CA_DIR, "2. Data Collection (Apify)",
                             "Comment Data Apify", "comments_merged.json")
SAMPLE_CSV = os.path.join(BASE_DIR, "sample_150.csv")    # the 150 posts drawn by a7

# Locate the claude CLI binary: prefer whatever is on PATH, else the usual install path.
CLAUDE_BIN = shutil.which("claude") or os.path.expanduser("~/.local/bin/claude")
LEAN_CWD = os.path.realpath(tempfile.gettempdir())  # run claude OUTSIDE the vault -> no CLAUDE.md overhead

# ---- model routing (CLI --model) --------------------------------------------
# NOTE (review): the module docstring above lists Stage 1/1b as claude-opus-4-8 and Stage 2
# as claude-fable-5, but the constants below route Stage 1/1b to sonnet-4-6 and Stage 2 to
# opus-4-8. Docstring and code disagree on which model coded the sample - reconcile before
# reporting the Phase B model in the methods section.
MODEL_STAGE1 = "claude-sonnet-4-6"  # per-post extraction + stance (lean; ~6x cheaper than Opus, quality holds)
MODEL_SYNTH = "claude-opus-4-8"     # single Stage-2 synthesis (Fable 5 not confirmed on subscription; 1 call)
SYNTH_FALLBACK = "claude-opus-4-8"  # model retried if the primary synthesis call comes back empty/errored
STANCE_MIN_COMMENTS = 3             # only classify stance for posts with at least this many comments
MAX_COMMENTS = 15                   # cap comments per post fed to the model (bounds prompt size)
CALL_RETRIES = 3                    # transient CLI/parse failures are retried this many times

# ---- output schemas (embedded in the prompt; parsed defensively) ------------
# These JSON Schemas define the exact object each model call must return. They are pasted
# into the prompt as the requested output contract, then the reply is parsed against them
# (the CLI cannot hard-force tool output, so the schema is described, not enforced).
# STAGE1_SCHEMA = the per-post open-coding fields (function, appeal, visual quality, ...).
STAGE1_SCHEMA = {
    "type": "object", "additionalProperties": False,
    "properties": {
        "functional_genre": {"type": "string", "description": "2-4 word label for the post's communicative FUNCTION, not its topic"},
        "genre_confidence": {"type": "string", "enum": ["high", "medium", "low"]},
        "speech_act": {"type": "string", "description": "inform|narrate|invite|request|announce|commemorate|celebrate|recruit|thank|promote|other"},
        "actual_cta": {"type": "string", "description": "the concrete ask verbatim, or 'none'"},
        "narrative_structure": {"type": "string", "description": "problem-solution|anecdote|explainer/list|showcase|milestone|none|other"},
        "image_role": {"type": "string", "enum": ["illustrative", "decorative", "text-carrier", "data-viz", "symbolic", "none"]},
        "image_caption_relation": {"type": "string", "enum": ["adds-info", "repeats", "unrelated", "contradicts"]},
        "target_audience": {"type": "string", "description": "tourists|donors|local-community|youth|peers/researchers|policymakers|general-public|supporters (multi ok)"},
        "appeal_type": {"type": "string", "description": "emotional|informational|social-normative|identity|aesthetic|economic|none (multi ok)"},
        "species_mentioned": {"type": "array", "items": {"type": "string"}},
        "missed_visual_opportunity": {"type": "object", "additionalProperties": False,
            "properties": {"flag": {"type": "boolean"}, "why": {"type": "string"}}, "required": ["flag", "why"]},
        "register_tone": {"type": "string", "description": "warm-personal|institutional-formal|playful|urgent|promotional|reverent"},
        "visual_quality": {"type": "object", "additionalProperties": False, "properties": {
            "aesthetic": {"type": "string", "enum": ["professional", "competent", "amateur", "screenshot-graphic"]},
            "production_effort": {"type": "string", "enum": ["high", "medium", "low"]},
            "human_presence": {"type": "string", "enum": ["none", "one-person", "group", "crowd"]},
            "people_role": {"type": "string"},
            "visual_species": {"type": "array", "items": {"type": "string"}},
            "colour_mood": {"type": "string"},
            "on_image_text_amount": {"type": "string", "enum": ["none", "minimal", "moderate", "text-dominant"]}},
            "required": ["aesthetic", "production_effort", "human_presence", "people_role", "visual_species", "colour_mood", "on_image_text_amount"]},
        "notes": {"type": "string"},
    },
    "required": ["functional_genre", "genre_confidence", "speech_act", "actual_cta", "narrative_structure",
                 "image_role", "image_caption_relation", "target_audience", "appeal_type", "species_mentioned",
                 "missed_visual_opportunity", "register_tone", "visual_quality", "notes"],
}

# STANCE_SCHEMA = per-comment stance coding (support/criticism/...) plus one dominant stance.
STANCE_SCHEMA = {
    "type": "object", "additionalProperties": False,
    "properties": {
        "comments": {"type": "array", "items": {"type": "object", "additionalProperties": False, "properties": {
            "idx": {"type": "integer"},
            "stance": {"type": "string", "enum": ["support", "criticism", "question", "off-topic", "misinformation", "spam"]},
            "engages_frame": {"type": "boolean"},
            "affect": {"type": "string", "enum": ["positive", "negative", "neutral"]}},
            "required": ["idx", "stance", "engages_frame", "affect"]}},
        "dominant_stance": {"type": "string", "enum": ["support", "criticism", "question", "off-topic", "misinformation", "spam", "mixed"]},
    },
    "required": ["comments", "dominant_stance"],
}

# ---- system prompts (the instruction block prepended to each model call) -----
# SYSTEM_STAGE1 = coder instructions for the per-post extraction (function over topic).
SYSTEM_STAGE1 = (
    "You are a content analyst building a data-grounded typology of how Namibian conservation "
    "organisations communicate on Instagram. You are shown one post: its image and its caption. Describe "
    "its communicative FUNCTION, not just its subject. Many posts are NOT persuasion. They inform, greet, "
    "promote a lodge, mark an awareness day, announce a rebrand, recruit volunteers, or advertise a live "
    "episode. Name what you actually see. Prefer a short, reusable label over a bespoke phrase, but do not "
    "force a post into a persuasion frame that is not there. If the post has no call to action, say 'none'. "
    "Judge the image on its own terms: does it add information, merely repeat the caption, carry text, show "
    "data, or is it decorative? Rate production quality, who if anyone is shown, and which species are "
    "actually visible in the frame (which may differ from the caption). Flag a missed visual opportunity only "
    "when the caption makes a specific, showable claim (a number, a place, a before/after) that the image ignores."
)
# SYSTEM_STANCE = coder instructions for comment stance (stance toward the org, not sentiment).
SYSTEM_STANCE = (
    "You classify Instagram comment stance for a content analysis. For each comment, classify its stance "
    "toward the POST: support (endorses/appreciates), criticism (challenges the org/message), question, "
    "off-topic, misinformation (false claim), or spam. Emotionally negative wording (grief, anger at "
    "poachers, 'humans are the problem') is usually SUPPORT for the conservation mission, not criticism; "
    "classify by stance toward the org, not by sentiment. Mark whether each comment engages the post's message."
)
# SYSTEM_SYNTH = Stage-2 instructions: cluster all the open labels into a consolidated codebook.
SYSTEM_SYNTH = (
    "You are consolidating an inductively coded content-analysis sample into a clean, data-grounded typology. "
    "You are given, for ~150 Namibian conservation Instagram posts, the open labels a first-pass coder assigned "
    "on four dimensions: functional_genre, actual_cta, narrative_structure, and appeal_type. For EACH dimension, "
    "cluster the raw labels into a small consolidated set of categories. For each category give: a NAME, a "
    "one-sentence definition, brief inclusion/exclusion notes, an approximate count, and 2-3 example post_ids. "
    "Then add a short 'Reading' paragraph per dimension. Output clean Markdown only; this becomes the codebook "
    "that replaces the a-priori scheme for the exploratory analysis. Do not invent posts or ids."
)


def shortcode(url):
    """Extract the Instagram shortcode (the '/p/<code>/' path segment) from a post URL.
    Used to key comments by Post_ID. Returns None when the URL has no shortcode."""
    m = re.search(r"/p/([^/]+)/", str(url))
    return m.group(1) if m else None


def load_inputs():
    """Load the three Phase-B inputs: the 150-post sample DataFrame, a {Post_ID: caption}
    dict, and a {Post_ID: [comment texts]} dict keyed by shortcode."""
    import pandas as pd
    sample = pd.read_csv(SAMPLE_CSV)
    nlp = pd.read_csv(CAPTION_CSV, sep=";", comment="#", encoding="utf-8-sig")
    caps = nlp.set_index("Post_ID")["Caption_Text"].fillna("").astype(str).to_dict()
    with open(COMMENTS_JSON, encoding="utf-8") as f:
        raw = json.load(f)
    comments = {}
    # Group the flat list of comment records into lists keyed by the post's shortcode.
    for c in raw:
        pid = shortcode(c.get("postUrl"))
        if pid:
            comments.setdefault(pid, []).append(str(c.get("text", "")).strip())
    return sample, caps, comments


def cached(pid, stage):
    """Return the cached parsed result for this post+stage if it exists, else None.
    This cache is what makes a re-run free: an already-coded post is read, not re-called."""
    p = os.path.join(CACHE_DIR, f"{pid}_{stage}.json")
    if os.path.exists(p):
        with open(p, encoding="utf-8") as f:
            return json.load(f)
    return None


def cache_write(pid, stage, obj):
    """Persist one post's parsed result to output/llm_cache/<pid>_<stage>.json."""
    os.makedirs(CACHE_DIR, exist_ok=True)
    with open(os.path.join(CACHE_DIR, f"{pid}_{stage}.json"), "w", encoding="utf-8") as f:
        json.dump(obj, f, ensure_ascii=False, indent=2)


# Running tally of API-equivalent cost + call count; mutated in place by cli_call().
COST = {"usd": 0.0, "calls": 0}


def cli_call(prompt, model, allow_read):
    """One headless `claude -p` call on the subscription. Returns (result_text, cost_usd).
    Lean config: run outside the vault (no CLAUDE.md), no MCP servers, no bundled skills, Read-only."""
    # Build the headless CLI call: -p = print/non-interactive, JSON envelope out, chosen
    # model, and --strict-mcp-config to ignore any ambient MCP servers (lean, deterministic).
    args = [CLAUDE_BIN, "-p", "--output-format", "json", "--model", model, "--strict-mcp-config"]
    if allow_read:
        # Vision/stance stages must read the jpg: allow ONLY the Read tool, scoped to media_test.
        args += ["--allowedTools", "Read", "--add-dir", MEDIA_DIR]
    env = dict(os.environ, CLAUDE_CODE_DISABLE_BUNDLED_SKILLS="1")   # drop bundled skills to cut overhead
    # Feed the prompt on stdin; cwd is the temp dir (outside the vault) so no CLAUDE.md loads.
    r = subprocess.run(args, input=prompt, capture_output=True, text=True, cwd=LEAN_CWD, env=env)
    if r.returncode != 0:
        raise RuntimeError(f"claude cli rc={r.returncode}: {(r.stderr or r.stdout)[-300:]}")  # non-zero exit -> show tail
    env_obj = json.loads(r.stdout)                # the CLI's JSON envelope
    if env_obj.get("is_error"):
        raise RuntimeError(f"claude cli error: {str(env_obj.get('result'))[:300]}")  # ran but reported a model/tool error
    COST["usd"] += float(env_obj.get("total_cost_usd") or 0.0)   # accumulate reported cost
    COST["calls"] += 1
    return env_obj.get("result", ""), float(env_obj.get("total_cost_usd") or 0.0)


def extract_json(text):
    """Pull the first JSON object out of a model reply. Strips ``` fences if present, then
    slices from the first '{' to the last '}' (defensive: the model may wrap JSON in prose)."""
    text = (text or "").strip()
    if text.startswith("```"):
        text = re.sub(r"^```[a-zA-Z]*\n?", "", text)     # drop the opening fence (```json etc.)
        text = re.sub(r"\n?```$", "", text).strip()      # drop the closing fence
    a, b = text.find("{"), text.rfind("}")               # outermost brace positions
    if a == -1 or b == -1:
        raise ValueError(f"no JSON object in output: {text[:200]}")
    return json.loads(text[a:b + 1])


def model_json(system, user_text, image_path, schema, model):
    """Assemble the full prompt (system + schema + optional image path + user text), call the
    CLI, and parse the JSON reply. Retries up to CALL_RETRIES times, re-raising the last error
    if every attempt fails."""
    prompt = (system + "\n\nReturn ONLY a single raw JSON object (no markdown fences, no prose) that "
              "conforms to this JSON Schema:\n" + json.dumps(schema))
    if image_path:
        prompt += f"\n\nFirst read the image at this absolute path, then analyse it:\n{image_path}"   # tell it to Read the jpg
    prompt += "\n\n" + user_text
    last = None
    for _ in range(CALL_RETRIES):
        try:
            result, _cost = cli_call(prompt, model, allow_read=bool(image_path))   # Read enabled only when there is an image
            return extract_json(result)
        except Exception as e:
            last = e                                # remember the failure and retry
    raise last                                      # all retries exhausted -> surface the last error


def run_stage1(sample, caps):
    """Stage 1: one open-extraction call per post (image + caption -> a Stage-1 label set).
    Cached posts are reused for free; a failed call is logged and skipped (retried on relaunch)."""
    print(f"\n[Stage 1] per-post extraction on {MODEL_STAGE1} ({len(sample)} posts)", flush=True)
    rows = []
    for i, r in enumerate(sample.itertuples(), 1):
        pid = r.Post_ID
        obj = cached(pid, "stage1")                # reuse a prior result if one is cached
        if obj is None:
            try:
                obj = model_json(SYSTEM_STAGE1, f"Caption:\n{caps.get(pid, '')}",
                                 os.path.join(MEDIA_DIR, f"{pid}.jpg"), STAGE1_SCHEMA, MODEL_STAGE1)
                cache_write(pid, "stage1", obj)    # persist so a re-run skips this call
                tag = "new"
            except Exception as e:
                print(f"  {i:3d}/{len(sample)}  {pid}  FAILED: {str(e)[:120]} (will retry on relaunch)", flush=True)
                continue                           # skip this post; a later relaunch retries it
        else:
            tag = "cache"
        obj["post_id"] = pid                       # stamp the id onto the result row
        rows.append(obj)
        if tag == "new" or i % 10 == 0:            # progress line: every new call + every 10th post
            print(f"  {i:3d}/{len(sample)}  {pid}  [{tag}]  genre={str(obj.get('functional_genre',''))[:30]}"
                  f"   ~${COST['usd']:.2f} used", flush=True)
    return rows


def run_stance(sample, caps, comments):
    """Stage 1b: classify comment stance, but only for posts with >= STANCE_MIN_COMMENTS
    scraped comments (fewer than that is not worth a call)."""
    eligible = [r.Post_ID for r in sample.itertuples()
                if len(comments.get(r.Post_ID, [])) >= STANCE_MIN_COMMENTS]
    print(f"\n[Stage 1b] comment stance on {MODEL_STAGE1} ({len(eligible)} posts >= {STANCE_MIN_COMMENTS} comments)", flush=True)
    rows = []
    for i, pid in enumerate(eligible, 1):
        obj = cached(pid, "stage1b")
        if obj is None:
            cl = comments.get(pid, [])[:MAX_COMMENTS]   # cap the number of comments fed to the model
            listing = "\n".join(f"[{j}] {t}" for j, t in enumerate(cl))   # number them so the model can index each
            try:
                obj = model_json(SYSTEM_STANCE, f"Caption:\n{caps.get(pid,'')}\n\nComments:\n{listing}",
                                 None, STANCE_SCHEMA, MODEL_STAGE1)   # image_path=None: stance is text-only
                cache_write(pid, "stage1b", obj)
                tag = "new"
            except Exception as e:
                print(f"  {i:3d}/{len(eligible)}  {pid}  FAILED: {str(e)[:120]}", flush=True)
                continue
        else:
            tag = "cache"
        obj["post_id"] = pid
        rows.append(obj)
        if tag == "new" or i % 10 == 0:
            print(f"  {i:3d}/{len(eligible)}  {pid}  [{tag}]  dominant={obj.get('dominant_stance','')}"
                  f"   ~${COST['usd']:.2f} used", flush=True)
    return rows


def run_synth(stage1_rows):
    """Stage 2: a single synthesis call that consolidates all Stage-1 open labels into a clean
    codebook (Markdown). Only 4 dimensions are sent, to keep the one big prompt compact."""
    print(f"\n[Stage 2] taxonomy synthesis on {MODEL_SYNTH} (1 call, fallback {SYNTH_FALLBACK})", flush=True)
    # Slim each post down to the 4 dimensions being clustered (genre, CTA, narrative, appeal).
    payload = [{"post_id": r["post_id"], "functional_genre": r.get("functional_genre", ""),
                "actual_cta": r.get("actual_cta", ""), "narrative_structure": r.get("narrative_structure", ""),
                "appeal_type": r.get("appeal_type", "")} for r in stage1_rows]
    user = ("Here are the open labels for each post as JSON lines. Consolidate each dimension into a clean "
            "taxonomy per the instructions.\n\n" + "\n".join(json.dumps(p, ensure_ascii=False) for p in payload))
    prompt = SYSTEM_SYNTH + "\n\n" + user
    text, used = "", MODEL_SYNTH
    try:
        text, _c = cli_call(prompt, MODEL_SYNTH, allow_read=False)
    except Exception as e:
        print(f"  {MODEL_SYNTH} failed ({str(e)[:120]}); falling back to {SYNTH_FALLBACK}", flush=True)
    if not text.strip():                           # empty or errored -> retry once on the fallback model
        text, _c = cli_call(prompt, SYNTH_FALLBACK, allow_read=False)
        used = SYNTH_FALLBACK
    # Write the consolidated codebook; it replaces the a-priori scheme for the exploratory analysis.
    with open(os.path.join(BASE_DIR, "taxonomy_synthesis.md"), "w", encoding="utf-8") as f:
        f.write(f"# Study 1 - Inductive taxonomy synthesis\n\n*Generated by `b1_llm_taxonomy_pass.py` "
                f"(Stage 2, model {used}) over {len(payload)} posts.*\n\n{text}\n")
    print(f"  wrote taxonomy_synthesis.md (model {used}, {len(text)} chars)", flush=True)


def write_tables(stage1_rows, stance_rows):
    """Flatten the nested Stage-1 objects into llm_stage1.csv (one row per post, the nested
    visual_quality / missed_visual_opportunity fields unpacked into flat columns) and the
    stance objects into llm_stage1b_comments.csv (one row per comment)."""
    import pandas as pd
    if stage1_rows:
        flat = []
        for r in stage1_rows:
            # Copy every scalar field, holding back the two nested dicts to unpack next.
            d = {k: v for k, v in r.items() if k not in ("visual_quality", "missed_visual_opportunity")}
            for k, v in (r.get("visual_quality") or {}).items():
                d[f"vq_{k}"] = v                   # flatten visual_quality.* into vq_* columns
            mvo = r.get("missed_visual_opportunity") or {}
            d["missed_visual_flag"] = mvo.get("flag")
            d["missed_visual_why"] = mvo.get("why")
            for c in ("species_mentioned", "vq_visual_species"):
                if isinstance(d.get(c), list):
                    d[c] = "; ".join(map(str, d[c]))   # join list-valued cells into one string for CSV
            flat.append(d)
        pd.DataFrame(flat).to_csv(os.path.join(BASE_DIR, "llm_stage1.csv"), index=False)
        print(f"  wrote llm_stage1.csv ({len(flat)} rows)", flush=True)
    if stance_rows:
        long = []
        for r in stance_rows:
            for c in r.get("comments", []):
                if isinstance(c, dict):
                    long.append({"post_id": r["post_id"], **c})   # explode: one row per comment, tagged with its post
        pd.DataFrame(long).to_csv(os.path.join(BASE_DIR, "llm_stage1b_comments.csv"), index=False)
        print(f"  wrote llm_stage1b_comments.csv ({len(long)} comment rows)", flush=True)


def dry_run(sample, caps, comments):
    """Validate inputs without spending anything: report missing images/captions, how many
    posts are stance-eligible, how many are already cached, and whether the CLI is found."""
    print("\n=== DRY RUN (no calls, no subscription usage) ===")
    missing_img = [r.Post_ID for r in sample.itertuples()
                   if not os.path.exists(os.path.join(MEDIA_DIR, f"{r.Post_ID}.jpg"))]   # posts with no jpg on disk
    missing_cap = [r.Post_ID for r in sample.itertuples() if not caps.get(r.Post_ID)]   # posts with an empty caption
    stance_n = sum(1 for r in sample.itertuples() if len(comments.get(r.Post_ID, [])) >= STANCE_MIN_COMMENTS)
    done1 = sum(1 for r in sample.itertuples() if cached(r.Post_ID, "stage1"))   # already-coded posts (free on relaunch)
    print(f"sample rows           : {len(sample)}")
    print(f"images present        : {len(sample)-len(missing_img)}/{len(sample)}"
          + (f"  MISSING {missing_img}" if missing_img else "  (all present)"))
    print(f"captions present      : {len(sample)-len(missing_cap)}/{len(sample)}")
    print(f"stance-eligible posts : {stance_n}")
    print(f"already cached (stage1): {done1}/{len(sample)}  (a relaunch skips these free)")
    print(f"claude CLI            : {CLAUDE_BIN}  {'FOUND' if os.path.exists(CLAUDE_BIN) or shutil.which('claude') else 'NOT FOUND'}")
    print(f"model routing         : Stage1/1b={MODEL_STAGE1}  Stage2={MODEL_SYNTH} (fallback {SYNTH_FALLBACK})")
    print("engine                : claude CLI (subscription, no API key)")
    print("dry run OK.\n")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dry-run", action="store_true")    # validate inputs, make no calls
    ap.add_argument("--limit", type=int, default=None)   # cap posts processed (smoke test)
    ap.add_argument("--stage", type=str, default="all", help="all | 1 | 1b | 2")
    args = ap.parse_args()

    sample, caps, comments = load_inputs()
    if args.limit:
        sample = sample.head(args.limit)           # keep only the first N posts

    if args.dry_run:
        dry_run(sample, caps, comments)
        return
    if not (os.path.exists(CLAUDE_BIN) or shutil.which("claude")):
        sys.exit("ERROR: `claude` CLI not found on PATH.")

    stage1_rows, stance_rows = [], []
    # Stage 2 needs Stage-1 rows, so requesting stage "2" also runs Stage 1 (served from cache).
    if args.stage in ("all", "1", "2"):
        stage1_rows = run_stage1(sample, caps)
    if args.stage in ("all", "1b"):
        stance_rows = run_stance(sample, caps, comments)
    write_tables(stage1_rows, stance_rows)
    if args.stage in ("all", "2") and stage1_rows:
        run_synth(stage1_rows)
    print(f"\nDone. {COST['calls']} calls, ~${COST['usd']:.2f} subscription-equivalent usage.", flush=True)


if __name__ == "__main__":
    main()
