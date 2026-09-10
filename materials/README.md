# Materials

Everything here documents how the study was delivered rather than how it was
analyzed. Each item is present because a claim in the thesis can be checked
against it, not because it happened to exist.

## `landing_page/`

The page that carried the secondary outcome. Chapter 4 makes several checkable
claims about it, and the source is the only thing that can verify them: Google
Analytics 4 loads only after active consent, the cookie banner weighs *Accept
all* and *Reject all* equally alongside a *Customize* control, and fonts and
scripts are self-hosted so nothing third-party loads before consent. That last
claim is why `vendor/tailwind.js` and `fonts/` are here rather than a CDN link.

Open `index.html` in a browser to see what a visitor saw. Under 1 MB in total.
Decorative video, reference images and server backups are not included, and the
deployment credentials never leave the author's machine.

## `pretest/` and `exit_survey/`

Both instruments, each with its questionnaire rendered from the SoSci exports
that ship in `data/raw/`: every question, every item wording, and the meaning of
every response code. Being generated rather than transcribed, neither can drift
from the instrument that actually ran.

Both also have screenshots of the live pages, ten for the pretest and three for
the exit survey, the latter covering welcome and consent, the item matrix with
age, and the gender question.

## `stimuli/`

`ad-config.json` and `transcripts/` hold the canonical content of all twelve
advertisements. They are what verifies the design claim in Chapter 4: sections B
and D are word for word identical across all twelve, the manipulation sits in
section C alone, the same three hook clips serve all four conditions, and spoken
length runs from 32 words in the neutral version to 54 in the social norm one.

The twelve rendered advertisements are the files that ran in both campaign rounds.

**The stills and clips they were built from are not included and will not be.** The
repository ships the twelve rendered advertisements, their specification and their
transcripts, which is what the design claims in Chapter 4 can be checked against.

**The rendered videos are included in the repository.** The global `*.mp4` rule in `.gitignore`
carries an exception for `08_Reproduction/materials/stimuli/*.mp4`, so these twelve
files are tracked while the roughly 2.8 GB of raw production footage elsewhere in the
vault stays out.

**One decision remains and it is one-way.** At 365 MB the twelve files enter the
repository history permanently once committed, and git-lfs is not set up. Committing
them directly makes every future clone carry that weight and cannot be undone without
rewriting history. Installing git-lfs first and tracking `materials/stimuli/*.mp4`
through it avoids that, at the cost of a dependency anyone cloning the repository has
to have. A Zenodo deposit with a DOI cited in Appendix C is the third route and keeps
the repository light.
