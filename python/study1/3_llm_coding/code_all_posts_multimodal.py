#!/usr/bin/env python3
"""
MULTIMODAL LLM coding over the full 920-post corpus. For each post it fetches
the display image from the Instagram /embed/ endpoint (no Apify needed), then
codes all 19 variables with the claude CLI (MODEL = "sonnet", vision-capable)
using BOTH caption and image.

NOTE on media: one image per post = the DISPLAY image (video → cover frame,
carousel → first slide). So visual coding is partial for video/carousel.

Output: Study1_LLM_multimodal_920.csv  (Post_ID;Post_Format;MANUAL_<19>)
Images cached in media_test/, shared with the 184-post run.
"""
#
# PATHS IN THIS PACKAGE
# This script was written inside the thesis working tree, where its inputs sat in
# sibling folders named "2. Data Collection (Apify)" and the like. In the
# reproduction package the same files live under data/, and python/_paths.py
# holds the canonical locations.
#
# Status: RECORD ONLY. It cannot run here, because it needs a local language-model CLI and network access to fetch display images.
# The outputs it produced are shipped and are the canonical record, so nothing in
# the reported results depends on rerunning it.
#
# IMPORTANT: this auto-coder is NOT the kappa ground truth. The 184-post
# validation sample is coded BY HAND (blind coder.html) precisely so the
# human labels stay independent of any machine guess. This script produces the
# machine ("LLM") side that human coding is later compared against - it does not
# replace human coding.
#
# csv/json for IO, re to scrape the embed HTML, subprocess to call curl and the
# `claude` CLI, threading + ThreadPoolExecutor to code several posts at once, time
# for backoff sleeps.
import csv, json, os, re, subprocess, threading, time
from concurrent.futures import ThreadPoolExecutor, as_completed

HERE = os.path.dirname(os.path.abspath(__file__))
CORPUS = os.path.join(HERE, "..", "..", "2. Data Collection (Apify)", "Study1_Posts_RAW_20260520.csv")  # full 920-post corpus
MEDIA = os.path.join(HERE, "..", "media_test")   # image cache (shared with the 184 run)
OUT = os.path.join(HERE, "..", "Study1_LLM_multimodal_920.csv")   # per-post machine codes land here
MODEL = "sonnet"   # which Claude model the `claude` CLI is asked to use (vision-capable)
UA = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15"   # browser-like UA for IG /embed/
os.makedirs(MEDIA, exist_ok=True)
# Cooldown switch: set env MM_CACHE_ONLY=1 to code ONLY images already on disk and
# make zero new Instagram calls (useful when IG has started throttling this IP).
CACHE_ONLY = os.environ.get("MM_CACHE_ONLY") == "1"   # code only already-cached images, no IG calls

# The 19 codebook variables, grouped by type:
#   BIN = binary 0/1 frames (mostly caption-driven)
#   MUL = multiclass variables (exactly one label from a fixed set)
#   VIS = visual binary variables that require the IMAGE, not just the caption
# FIELDS is all 19 in a fixed order; MANUAL_<field> columns are written in this order.
BIN = ["Empathy","Threat","Efficacy","Collective_ID","Normative","Moral",
       "Economic","Scientific","Youth_Addressed","Interactivity"]
MUL = ["Emotional_Valence","Story_Structure","Primary_CTA","Primary_Topic","Language"]
VIS = ["Youth_Style","Protagonist","Onscreen_Text","Data_Visual"]
FIELDS = BIN + MUL + VIS

# The coding instructions handed to the model as the prompt. This mirrors the
# Study 1 codebook definitions so the machine codes on the same rules a human would.
# (Do not treat these machine codes as reliability ground truth - see NOTE at top.)
RULES = """You are an expert content-analysis coder for Namibian conservation Instagram posts.
You are given a post's CAPTION and its IMAGE (for videos this is the cover frame, for
carousels the first slide). Use BOTH. Code every variable. Frames are not mutually exclusive.

BINARY (0/1):
- Empathy: compassion/perspective-taking toward a specific individual (named animal/person), suffering or inner perspective.
- Threat: explicit danger, loss, decline, crisis, death, destruction, urgency.
- Efficacy: claims an action/approach works or will work.
- Collective_ID: shared "we"/community identity (not merely naming the org team).
- Normative: refers to others doing/expecting a behavior. If unsure, 0.
- Moral: duty, right/wrong, responsibility, ethical appeal.
- Economic: money, jobs, income, tourism value, livelihoods, funding as a framing argument.
- Scientific: data, research, monitoring, measurement, methods, tracking/collars/GPS.
- Youth_Addressed: explicitly addresses youth/students/learners/children (text).
- Interactivity: explicit prompt to tag/comment/share/vote. Usually 0.
VISUAL BINARY (judge from the IMAGE):
- Youth_Style: youthful/trendy visual style (memes, bold graphics, fast-edit/Reel aesthetic).
- Protagonist: an identifiable focal individual (a specific animal or person as the subject).
- Onscreen_Text: text burned into the image/frame (titles, captions, graphics). Caption does NOT count.
- Data_Visual: a data visualization in the image (chart, graph, map-with-data, statistics graphic).
MULTICLASS (exactly one listed string):
- Emotional_Valence: Distress | Hope | Neutral | Mixed
- Story_Structure: None | Problem | Solution | Problem-Solution
- Primary_CTA: Donate | Volunteer | Event | Share | Learn | Petition | None
- Primary_Topic: Wildlife | Anti-poaching | HWC | Community | Education | Research | Policy | Fundraising | Ecotourism | Climate
- Language: English | Afrikaans | Local | Mixed

Return ONLY a JSON object with all 19 fields (no id, no prose)."""


def _extract_img_url(emb):
    """Pull the display-image URL from raw /embed/ HTML (twin of prefetch's helper)."""
    # 1. video/carousel: display_url in the embedded JSON
    m = re.search(r'display_url\\?":\\?"(https.*?\.jpg.*?)"', emb)
    if m:
        return m.group(1).replace("\\u0026","&").replace("\\/","/").replace("\\","")
    # 2. single-image posts: the <img class="EmbeddedMediaImage" src="..."> tag
    m = (re.search(r'class="EmbeddedMediaImage"[^>]*\ssrc="([^"]+)"', emb)
         or re.search(r'src="([^"]+)"[^>]*class="EmbeddedMediaImage"', emb)
         # 3. last resort: any post-media URL (t51.*-15 path; -19 is the profile pic)
         or re.search(r'(https://scontent[^"\\ ]*t51\.[0-9]+-15/[^"\\ ]*\.jpg[^"\\ ]*)', emb))
    if m:
        return m.group(1).replace("&amp;","&").replace("\\u0026","&").replace("\\/","/")
    return None


def fetch_image(pid):
    """Return the local path to this post's cached display image, or None."""
    out = os.path.join(MEDIA, f"{pid}.jpg")
    if os.path.exists(out) and os.path.getsize(out) > 1000:
        return out               # already cached (shared cache with prefetch_post_images.py)
    if CACHE_ONLY:               # cooldown mode: do not touch Instagram, code cached only
        return None
    for attempt in range(1, 4):   # IG throttles concurrent embed fetches → retry with backoff
        # Grab the embed HTML, pull the image URL, download the JPG - same 3 steps as prefetch.
        emb = subprocess.run(["curl","-sL","-A",UA,f"https://www.instagram.com/p/{pid}/embed/"],
                             capture_output=True, text=True, timeout=60).stdout
        raw = _extract_img_url(emb)
        if raw:
            subprocess.run(["curl","-sL","-A",UA,raw,"-o",out], timeout=60)
            if os.path.exists(out) and os.path.getsize(out) > 1000:
                return out
        time.sleep(2 * attempt)   # grow the pause each attempt (2s, 4s) to ride out throttling
    return None


def claude(prompt):
    """Call the local `claude` CLI once and return its stdout text.

    -p = one-shot (print) mode; --model selects the model; stdin=DEVNULL so it
    never blocks waiting for interactive input; 180s cap per call.
    """
    r = subprocess.run(["claude","-p","--model",MODEL,prompt],
                       capture_output=True, text=True, timeout=180, stdin=subprocess.DEVNULL)
    return r.stdout.strip()


def parse_obj(text):
    """Extract the JSON object from the model's reply (it may wrap it in prose/fences)."""
    t = text
    if "```" in t:                     # strip a ```json ... ``` code fence if present
        t = t.split("```")[1]          # take what is between the first pair of fences
        if t.startswith("json"): t = t[4:]   # drop a leading "json" language tag
    a, b = t.find("{"), t.rfind("}")   # slice from the first { to the last } = the JSON body
    return json.loads(t[a:b+1])        # raises if it is not valid JSON (caught by the caller)


def code_post(post):
    """Code one post's 19 variables from its image + caption; return a row dict."""
    img = fetch_image(post["id"])
    # Every row records whether an image was available (image_ok), because visual
    # variables are only trustworthy when the model actually saw the frame.
    base = {"Post_ID": post["id"], "Post_Format": post["format"],
            "image_ok": "yes" if img else "no"}
    if not img:
        # No image -> return blank codes (all MANUAL_ empty). Caller treats this as a
        # failure and does not checkpoint it, so it is retried on a later run.
        return {**base, **{"MANUAL_"+f: "" for f in FIELDS}}
    # Prompt = "@<image path>" (the CLI attaches the file) + the codebook rules + caption.
    prompt = f"@{img}\n\n" + RULES + "\n\nCAPTION:\n" + (post["caption"] or "(none)")
    for attempt in range(1, 4):        # up to 3 tries (model reply may be unparseable/rate-limited)
        try:
            o = parse_obj(claude(prompt))   # ask the model, then parse its JSON
            row = dict(base)
            for f in FIELDS:
                v = o.get(f, "")
                # Sanitise the binary + visual fields: keep only a clean "0"/"1",
                # otherwise blank it (guards against the model returning prose/true/false).
                if f in BIN + VIS:
                    v = str(int(v)) if str(v).strip() in ("0","1") else ""
                row["MANUAL_"+f] = v        # store under the MANUAL_ prefix (matches the coding sheet)
            return row
        except Exception as e:
            if attempt == 3:               # give up after the third failure
                print(f"  ! {post['id']} parse fail: {e}")
                return {**base, **{"MANUAL_"+f: "" for f in FIELDS}}
            time.sleep(3 * attempt + 2)   # backoff to ride out transient rate limits


# Checkpoint file: one coded row per line as JSON (JSONL). It makes the run
# resumable - a crash/stop keeps everything already coded, so a re-run only does
# what is left. WORKERS = code 6 posts concurrently.
CKPT = os.path.join(HERE, "coding_checkpoint.jsonl")
WORKERS = 6

def main():
    import argparse
    # --limit N caps how many posts to code this run (0 = all).
    ap = argparse.ArgumentParser(); ap.add_argument("--limit", type=int, default=0); args = ap.parse_args()
    # Load the corpus (Post_ID + format + caption), keeping only rows with a Post_ID.
    with open(CORPUS, encoding="utf-8-sig", newline="") as fh:
        rows = [r for r in csv.DictReader(fh, delimiter=";") if (r.get("Post_ID") or "").strip()]
    posts = [{"id": r["Post_ID"], "format": r["Post_Format"], "caption": r["Caption_Text"] or ""} for r in rows]
    if args.limit:
        posts = posts[:args.limit]

    # Rebuild `done` from the checkpoint so already-coded posts are skipped.
    done = {}
    if os.path.exists(CKPT):
        for line in open(CKPT, encoding="utf-8"):
            try:
                o = json.loads(line); done[o["Post_ID"]] = o   # last line for a Post_ID wins
            except Exception:
                pass                                          # ignore a half-written final line
    todo = [p for p in posts if p["id"] not in done]          # only the not-yet-coded posts
    print(f"{len(posts)} posts | {len(done)} in ckpt | {len(todo)} to run ({WORKERS} parallel)", flush=True)

    # Shared lock guards both the checkpoint file and the progress counter across threads.
    lock = threading.Lock(); counter = [len(done)]            # single-element list = mutable counter for the closure
    ckpt_fh = open(CKPT, "a", encoding="utf-8")               # append mode: never clobber earlier progress
    def work(p):
        r = code_post(p)
        good = r.get("MANUAL_Empathy") in ("0", "1")   # only persist successfully coded rows; failures retry on resume
        with lock:
            if good:
                # Append + flush immediately so a crash still leaves this row on disk.
                ckpt_fh.write(json.dumps(r, ensure_ascii=False) + "\n"); ckpt_fh.flush()
            counter[0] += 1
            print(f"  done {counter[0]}/{len(posts)} ({p['format']}) img={r['image_ok']} ok={good}", flush=True)
        return r
    # Fan the todo posts across the pool; collect each result into `done` as it finishes.
    with ThreadPoolExecutor(max_workers=WORKERS) as ex:
        for fut in as_completed([ex.submit(work, p) for p in todo]):
            r = fut.result(); done[r["Post_ID"]] = r
    ckpt_fh.close()

    # Assemble the final CSV: header = context + MANUAL_<19>, rows in ORIGINAL corpus order.
    header = ["Post_ID","Post_Format","image_ok"] + ["MANUAL_"+f for f in FIELDS]
    ordered = [done[p["id"]] for p in posts if p["id"] in done]   # reorder `done` to match `posts`
    img_ok = sum(1 for r in ordered if r.get("image_ok") == "yes")   # how many had an image
    with open(OUT, "w", encoding="utf-8-sig", newline="") as fh:
        # First line is a "# ..." provenance banner; downstream readers (build_review_app)
        # skip lines starting with "#" so this comment does not become a data row.
        fh.write(f"# multimodal LLM TEST · model={MODEL}(vision) · display-image only · {len(ordered)} posts · {img_ok} with image\n")
        w = csv.DictWriter(fh, fieldnames=header, delimiter=";"); w.writeheader(); w.writerows(ordered)
    print(f"Wrote {OUT} | {img_ok}/{len(ordered)} had an image")


if __name__ == "__main__":
    main()