# Statistics Problem Generator

A [Shiny](https://shiny.posit.co/) web app that generates randomized, ready-to-solve statistics problems — complete with a data set, a visualization, and a full answer key — for practicing university-level statistics.

**Live app:** hosted via shinyapps.io
**Repo:** https://github.com/chadcwilliams/StatsProblemGenerator

---

## What is this?

If you're learning statistics, one of the best ways to actually get comfortable with a method (a t-test, an ANOVA, a chi-squared test, and so on) is to work through problem after problem by hand until the steps become second nature. Textbooks only have so many practice problems, and once you've seen the answer, that problem stops being useful.

This app solves that by generating a **new, randomized problem every time you click a button.** Pick a statistical test, hit *Generate Data*, and you get a fresh data set to work through — with the exact same structure you'd see on a homework set or exam, but numbers you've never seen before. When you're done (or stuck), you can reveal a complete, correctly-worked answer key, see a plot of the data, and download the whole thing as a clean PDF to print or save.

It was built with a two-course intro statistics sequence in mind (the dropdowns are labeled "Psyc 300A" and "Psyc 300B"), but the underlying tests cover most of what a first-year statistics course touches, from descriptive statistics through factorial ANOVA.

**Who it's for:**
- **Students** who want unlimited, self-checking practice problems for a specific method
- **Instructors / TAs** who want a quick way to generate extra practice sets, in-class examples, or exam-style questions — with reproducible versions they can regenerate later using a seed (more on that below)

---

## How to use it

The sidebar walks through the process in three numbered steps:

### 1. Choose a test

Two dropdowns list the available tests, grouped roughly the way a two-semester intro sequence would introduce them (see the full list below). Selecting a test in one dropdown automatically clears the other — only one test is active at a time.

### 2. Set sample parameters

- **Number of Participants** — a slider controlling the sample size used to generate the problem (this maps to *n* per group for multi-group tests).
- **Range of Values** — the slider range the randomly generated scores will be drawn from.
- **Seed (optional)** — see the [Reproducing a problem set](#reproducing-a-problem-set-the-seed) section below. Leave it blank unless you specifically want to reproduce or share an exact problem.

### 3. Run

- **Generate Data** — creates a new random problem and displays it in the *Data* panel. This is the button you'll click every time you want a fresh problem.
- **Plot Data** — renders a visualization appropriate to the test (a distribution curve, bar chart, scatterplot, and so on), the same kind of plot you'd be expected to sketch or produce for that analysis.
- **Show Answers** — reveals the fully worked answer key: every intermediate statistic, the test statistic itself, the p-value, and the final decision (reject/retain the null).
- **Download PDF** — appears once a data set has been generated. Downloads a three-page PDF containing the problem data, the plot, and the answer key, ready to print or archive. (See [The PDF report](#the-pdf-report) below.)

Switching to a different test at any point clears the data, plot, and answers on screen, so you always start clean.

---

## Reproducing a problem set (the seed)

Every problem is generated randomly, but sometimes you want to come back to the *exact same* problem later — to double-check your work, show a specific example to a class, or send a colleague the same problem you're looking at.

That's what the **Seed** box is for:

- **Leave it blank** (the normal case): the app still picks a random seed behind the scenes every time you click *Generate Data*, and displays it under the input box as **"Seed used: ######."** Write that number down (or just keep the PDF, which stamps it in a footnote on every page) and you can reproduce this exact problem set anytime in the future.
- **Type a specific number** into the box before clicking *Generate Data*: the app uses exactly that seed instead of picking a random one, so you'll get the identical problem set every time you use that number.

Either way, the seed used is also baked into the downloaded PDF's filename (e.g. `One_Way_ANOVA_482913.pdf`), so the file itself tells you how to regenerate it.

---

## The PDF report

Clicking **Download PDF** produces a three-page document:

1. **Problem Data** — the raw data/summary table exactly as shown on screen, ready to be handed to a student as a worksheet.
2. **Plot** — the corresponding visualization for that test.
3. **Answer Key** — every statistic computed along the way, through to the final decision.

Each page includes a small footnote with the seed used, so a printed or saved copy is always traceable back to a reproducible problem set.

---

## Available tests

<table>
<tr><th colspan="2">Psyc 300A</th></tr>
<tr><td>Frequency Distribution</td><td>Building and reading a frequency table</td></tr>
<tr><td>Descriptives</td><td>Mode, median, mean, range, SIQR, MAD, variance, SD, skew</td></tr>
<tr><td>Z-scores</td><td>Converting a raw score to/from a z-score</td></tr>
<tr><td>Correlation & Regression</td><td>Pearson's r and the regression line</td></tr>
<tr><td>Single Participant Z-Test</td><td>Comparing one person's score to a known population</td></tr>
<tr><td>Single Sample Z-Test</td><td>Comparing a sample mean to a known population (σ known)</td></tr>
<tr><td>Single Sample T-Test</td><td>Comparing a sample mean to a known population (σ unknown)</td></tr>
</table>

<table>
<tr><th colspan="2">Psyc 300B</th></tr>
<tr><td>Related Samples T-Test</td><td>Paired/repeated-measures comparison</td></tr>
<tr><td>Independent Samples T-Test</td><td>Comparing two independent groups</td></tr>
<tr><td>Correlation (Advanced)</td><td>Testing the significance of a correlation</td></tr>
<tr><td>Power (calculate n)</td><td>Solving for the sample size needed to hit a target power</td></tr>
<tr><td>Power (calculate power)</td><td>Solving for the power of a given design</td></tr>
<tr><td>One-Way ANOVA</td><td>Comparing 3+ group means</td></tr>
<tr><td>Multiple Comparisons</td><td>Planned comparisons and post-hoc tests (Tukey, Dunnett, Fisher's LSD, Dunn's test) following an ANOVA</td></tr>
<tr><td>Multifactorial ANOVA</td><td>Two-way (factorial) ANOVA with main effects and an interaction</td></tr>
<tr><td>Chi-Squared (Goodness of Fit)</td><td>Testing observed vs. expected category frequencies</td></tr>
<tr><td>Chi-Squared (Homogeneity & Independence)</td><td>Testing association between two categorical variables</td></tr>
</table>

---

## Under the hood

For anyone curious how the app is put together (or looking to extend it), here's the shape of the codebase:

```
app.R                          Entry point: wires up the UI, server logic,
                                test-switching, and the PDF download handler
utils/
  ui.R                         All UI layout, styling, and inputs
  observe_events.R             Renders the on-screen Answer Key and Plot for
                                whichever test is currently active
  pdf_report.R                 Builds the downloadable PDF: mirrors the
                                on-screen answer-key/plot logic so the PDF
                                and the screen always match
  stat_tests/
    freq_distribution.R
    descriptives.R
    ...(one file per test)     Each file generates that test's random data,
                                computes every statistic, and populates the
                                three shared pieces of state below
```

Each test script populates three shared pieces of reactive state that the rest of the app reads from:

- **`stats`** — every computed statistic for the answer key
- **`plotdata`** — whatever the plot needs to draw
- **`problemdata`** — the raw problem table shown on the *Data* panel (and reused by the PDF)

Because every test funnels into the same three objects, the answer-key rendering, plotting, and PDF-generation code only has to be written once and works for any test, rather than being duplicated 17 times.

The PDF itself is built with base R's `grid`/`gridExtra` packages rather than `rmarkdown`, specifically so it doesn't need Pandoc or a LaTeX installation on the hosting server — just plain R graphics.

---

## Running it locally

```r
# From the project root, with the working directory set to the app folder:
shiny::runApp()
```

Required packages: `shiny`, `bslib`, `rhandsontable`, `ggplot2`, `dplyr`, `gridExtra`, `faux`, `rstatix`, `BSDA`.

---

## Credits

Written by Chad C. Williams — [chadcwilliams.com](https://www.chadcwilliams.com)
