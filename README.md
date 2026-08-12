# Statistics Problem Generator

Shiny application that generates randomized statistics problems for practice: a data set, a plot, and an answer key, for a selected statistical test.

App: https://chadcwilliams.shinyapps.io/StatsProblemGenerator/
Repo: https://github.com/chadcwilliams/StatsProblemGenerator

## Overview

The app selects a statistical test, generates a random data set matching that test's requirements, computes every statistic required to solve it, and displays three outputs: the data, a plot, and the worked solution. Each click of "Generate Data" produces a new data set. A seed value can be set to reproduce a specific data set later.

The dropdowns are labeled "Psyc 300A" and "Psyc 300B" and split the 17 supported tests across a two-course sequence.

## Usage

Open the app: https://chadcwilliams.shinyapps.io/StatsProblemGenerator/

1. **Select a test.** Choose one option from either dropdown. Selecting a test in one dropdown clears the other; only one test is active at a time.
2. **Set parameters.**
   - *Number of Participants* — sample size used to generate the data (per group, for multi-group tests).
   - *Range of Values* — range the generated scores are drawn from.
   - *Seed* — optional. See [Seed](#seed).
3. **Generate Data.** Produces a data set and displays it in the Data panel.
4. **Plot Data.** Renders the plot associated with the selected test.
5. **Show Answers.** Displays the worked solution: intermediate statistics, the test statistic, the p-value, and the decision (reject or retain the null).
6. **Download PDF.** Available once a data set exists. Downloads a three-page PDF: problem data, plot, answer key. See [PDF output](#pdf-output).

Selecting a different test clears the data, plot, and answers currently on screen.

## Seed

Each data set is generated using a seed value passed to `set.seed()`.

- If the Seed field is left blank, the app generates a random seed on each click of "Generate Data" and displays it below the field as "Seed used: <value>".
- If a number is entered in the Seed field before clicking "Generate Data", that value is used instead.

Recording the seed value allows the same data set to be regenerated later by entering it into the Seed field. The seed used is also included in the PDF filename and as a footnote on each PDF page.

## PDF output

The downloaded PDF contains three pages:

1. Problem data, as shown in the Data panel.
2. Plot, as shown after clicking "Plot Data".
3. Answer key, as shown after clicking "Show Answers".

Each page includes a footnote with the seed used to generate the data set. The filename follows the pattern `<test_name>_<seed>.pdf`.

## Supported tests

### Psyc 300A

| Test | Description |
|---|---|
| Frequency Distribution | Frequency table construction |
| Descriptives | Mode, median, mean, range, SIQR, MAD, variance, SD, skew |
| Z-scores | Conversion between raw scores and z-scores |
| Correlation & Regression | Pearson's r and the regression line |
| Single Participant Z-Test | One participant's score vs. a known population |
| Single Sample Z-Test | Sample mean vs. a known population, σ known |
| Single Sample T-Test | Sample mean vs. a known population, σ unknown |

### Psyc 300B

| Test | Description |
|---|---|
| Related Samples T-Test | Paired/repeated-measures comparison |
| Independent Samples T-Test | Two independent groups |
| Correlation (Advanced) | Significance test of a correlation coefficient |
| Power (calculate n) | Sample size required for a target power |
| Power (calculate power) | Power of a specified design |
| One-Way ANOVA | Comparison of 3+ group means |
| Multiple Comparisons | Planned comparisons and post-hoc tests (Tukey, Dunnett, Fisher's LSD, Dunn's test) |
| Multifactorial ANOVA | Two-way ANOVA with main effects and an interaction |
| Chi-Squared (Goodness of Fit) | Observed vs. expected category frequencies |
| Chi-Squared (Homogeneity & Independence) | Association between two categorical variables |

## File structure

```
app.R                     Server/UI setup, test switching, PDF download handler
utils/
  ui.R                    UI layout and inputs
  observe_events.R        Renders the on-screen Answer Key and Plot for the active test
  pdf_report.R            Builds the PDF; mirrors the on-screen answer key/plot logic
  stat_tests/
    freq_distribution.R
    descriptives.R
    ...                   One file per test. Generates the data, computes the
                           statistics, and populates stats/plotdata/problemdata
```

Each test script writes to three shared reactive objects:

- `stats` — computed statistics for the answer key
- `plotdata` — data required by the plot
- `problemdata` — the problem table shown in the Data panel and reused in the PDF

Answer-key rendering, plotting, and PDF generation read from these three objects and are not duplicated per test.

The PDF is built with `grid`/`gridExtra` rather than `rmarkdown`, to avoid a Pandoc/LaTeX dependency on the hosting server.

## Local development

R packages: `shiny`, `bslib`, `rhandsontable`, `ggplot2`, `dplyr`, `gridExtra`, `faux`, `rstatix`, `BSDA`.

```r
shiny::runApp()
```

Run from the project root (the directory containing `app.R`).

## Author

Chad C. Williams — chadcwilliams.com
