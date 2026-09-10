# Exit survey

The voluntary survey on the landing page. It supplied the individual-difference
moderators for H2a, H2b and H6 and plays no part in the primary
click-through comparison. Its fifth item, horizontal collectivism, is
reported descriptively only.

## What is here

`screenshots/` shows the instrument as respondents met it, three pages covering
the whole survey:

| Page | Contents | Variables |
|---|---|---|
| `page_1.png` | Welcome text, purpose, voluntariness, data statement, contact, and the consent gate | `IT01`, `IT02` |
| `page_2.png` | The five-item matrix on a fully labelled 5-point agree scale, then age | `IM01_01` to `IM01_05`, `DM01` |
| `page_3.png` | Gender identity | `DM02` |

`instrument.md` carries the same content as text: every question, every item
wording, and the meaning of every response code. It was read out of the SoSci
exports in `data/raw/ExitSurvey/`, which carry the question text, every variable
label and the meaning of every response code, so it can be checked against them.
It is searchable where a screenshot is not.

One difference between the two records is worth knowing. The gender question
offers five options on screen, the fifth being a free-text *Prefer to
self-describe*. Only four carry a numeric code, so `instrument.md` lists four
and the screenshot shows five.

## Where the items come from

Each item was taken from a published instrument. Table 15 of the thesis reports
this mapping, and it is repeated here so the package carries it too.

| Item | Construct | Source instrument |
|---|---|---|
| `IM01_01` | State empathic concern | Davis (1983) IRI EC; Batson (2011) |
| `IM01_02` | State character identification | Cohen (2001) Identification Scale |
| `IM01_03` | General environmental concern | Dunlap et al. (2000) NEP |
| `IM01_04` | Domain-specific biospheric value salience | De Groot and Steg (2008) biospheric subscale |
| `IM01_05` | Horizontal collectivism | Singelis et al. (1995) INDCOL HC |

## Responses

The 13 completions are in `data/processed/exitsurvey_clean.csv`, the raw SoSci
exports in `data/raw/ExitSurvey/`. No identifying information was collected, as
the welcome page states.
