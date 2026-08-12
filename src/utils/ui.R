####################################################################
####          Modernized interface — bslib / Bootstrap 5        ####
####          All inputIds / outputIds unchanged from original  ####
####################################################################

library(bslib)

app_theme = bs_theme(
  version = 5,
  base_font = font_google("Inter"),
  heading_font = font_google("Inter", wght = 600),
  bg = "#F7F8FA",
  fg = "#1F2430",
  primary = "#4C6EF5",
  secondary = "#748094",
  success = "#2FB380",
  info = "#4C6EF5",
  "body-color" = "#1F2430",
  "border-radius" = "0.6rem",
  "border-radius-lg" = "0.85rem"
) |>
  bs_add_rules("
    .navbar-brand { font-weight: 700; letter-spacing: -0.01em; }

    .sidebar-card {
      position: sticky;
      top: 1rem;
    }

    .card {
      border: 1px solid rgba(31, 36, 48, 0.06);
      box-shadow: 0 1px 2px rgba(16, 24, 40, 0.04), 0 1px 3px rgba(16, 24, 40, 0.03);
      background-color: #ffffff;
    }

    .card-header {
      background-color: #ffffff;
      border-bottom: 1px solid rgba(31, 36, 48, 0.06);
      font-weight: 600;
      font-size: 0.95rem;
      display: flex;
      align-items: center;
      gap: 0.5rem;
      letter-spacing: -0.01em;
    }

    .form-label {
      font-weight: 500;
      font-size: 0.85rem;
      color: #3A4256;
      margin-bottom: 0.35rem;
    }

    .form-select, .selectize-input {
      border-radius: 0.5rem !important;
      border-color: rgba(31, 36, 48, 0.14) !important;
    }

    /* Make placeholder text (e.g. the '1234' seed example) clearly
       paler than real input text, so it reads as a hint rather than
       an actual value. */
    .form-control::placeholder {
      color: rgba(31, 36, 48, 0.14) !important;
      opacity: 1;
    }

    .btn {
      border-radius: 0.55rem;
      font-weight: 500;
      font-size: 0.9rem;
      padding: 0.5rem 0.9rem;
    }

    .btn-toolbar-stack {
      display: flex;
      flex-direction: column;
      gap: 0.5rem;
      margin-top: 0.5rem;
    }

    .irs--shiny .irs-bar {
      background: #4C6EF5;
      border-top: 1px solid #4C6EF5;
      border-bottom: 1px solid #4C6EF5;
    }
    .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to {
      background-color: #4C6EF5;
    }
    .irs--shiny .irs-handle > i:first-child {
      background-color: #4C6EF5;
    }

    .irs { max-width: 946px; }

    hr.divider {
      margin: 0.5rem 0;
      border-top: 1px solid rgba(31, 36, 48, 0.08);
    }

    .app-subtitle {
      color: #748094;
      font-size: 0.85rem;
      margin-top: -0.4rem;
      margin-bottom: 0.75rem;
    }

    .repo-link {
      color: #8A93A6;
      font-size: 0.8rem;
      font-weight: 500;
      text-decoration: none;
      display: inline-flex;
      align-items: center;
      gap: 0.35rem;
      padding: 0.2rem 0.5rem;
      border-radius: 0.4rem;
      transition: color 0.15s ease, background-color 0.15s ease;
    }
    .repo-link:hover {
      color: #4C6EF5;
      background-color: rgba(76, 110, 245, 0.08);
      text-decoration: none;
    }

    .card-body {
      padding: 0.3rem 0.5rem !important;
    }

    .card-header {
      padding: 0.3rem 0.5rem;
      font-size: 0.85rem;
    }

    .content-panel {
      background: #ffffff;
      border-radius: 0.6rem;
      padding: 0.5rem;
    }

    #data_display, #stats_display {
      overflow: auto;
      height: 100%;
    }

    #data_display .handsontable, #stats_display .handsontable {
      font-size: 0.85rem;
    }

    #distribution_display {
      height: 100% !important;
    }
  ")

UI = function(){
  page_fillable(
    theme = app_theme,
    title = "Stats Toolkit",
    padding = c(6, 8),
    fillable_mobile = TRUE,

    div(
      class = "mb-1 d-flex justify-content-between align-items-center",
      h5("Statistics Problem Generator", class = "mb-0 fw-semibold"),
      tags$a(
        href = "https://github.com/chadcwilliams/StatsProblemGenerator",
        target = "_blank",
        rel = "noopener noreferrer",
        class = "repo-link",
        icon("github"),
        "Go to repo"
      )
    ),

    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 320,
        class = "sidebar-card",
        open = "open",

        div(class = "fw-semibold mb-2", "1. Choose a test"),
        selectInput(
          "Test_300A",
          label = "Psyc 300A",
          choices = c(
            " " = "",
            "Frequency Distribution" = 1,
            "Descriptives" = 2,
            "Correlation & Regression" = 4,
            "Z-scores" = 17,
            "Single Participant Z-Test" = 3,
            "Single Sample Z-Test" = 5,
            "Single Sample T-Test" = 6
          ),
          selected = ""
        ),
        selectInput(
          "Test_300B",
          label = "Psyc 300B",
          choices = c(
            " " = "",
            "Related Samples T-Test" = 7,
            "Correlation (Advanced)" = 8,
            "Independent Samples T-Test" = 9,
            "Power (calculate n)" = 10,
            "Power (calculate power)" = 11,
            "One-Way ANOVA" = 12,
            "Multiple Comparisons" = 13,
            "Multifactorial ANOVA" = 14,
            "Chi-Squared (Goodness of Fit)" = 15,
            "Chi-Squared (Homogeneity & Independence)" = 16
          ),
          selected = ""
        ),

        tags$hr(class = "divider"),

        div(class = "fw-semibold mb-2", "2. Set sample parameters"),
        sliderInput(
          inputId = 'num_of_participants',
          label = 'Number of Participants',
          value = 10,
          min = 2,
          max = 100,
          step = 1,
          width = '100%'
        ),
        sliderInput(
          inputId = 'value_range',
          label = 'Range of Values',
          value = c(1, 10),
          min = 1,
          max = 100,
          step = 1,
          width = '100%'
        ),
        textInput(
          inputId = 'seed',
          label = 'Seed (optional)',
          value = '',
          placeholder = '1234',
          width = '100%'
        ),
        uiOutput('seed_display'),

        tags$hr(class = "divider"),

        div(class = "fw-semibold mb-2", "3. Run"),
        div(
          class = "btn-toolbar-stack",
          actionButton('refresh', 'Generate Data', icon = icon("rotate"), class = "btn-primary"),
          actionButton('distribution', 'Plot Data', icon = icon("chart-line"), class = "btn-outline-secondary"),
          actionButton('answers', 'Show Answers', icon = icon("check"), class = "btn-outline-secondary"),
          uiOutput('download_pdf_slot')
        )
      ),

      div(
        class = "content-panel",
        style = "display: flex; flex-direction: column; gap: 0.4rem; height: 100%;",

        div(
          style = "display: flex; flex-direction: row; gap: 0.4rem; height: 50%;",

          card(
            full_screen = TRUE,
            style = "flex: 1 1 50%;",
            card_header(
              "Data"
            ),
            card_body(rHandsontableOutput("data_display", height = "100%"), class = "p-2")
          ),

          card(
            full_screen = TRUE,
            style = "flex: 1 1 50%;",
            card_header(
              "Figure 1"
            ),
            card_body(plotOutput('distribution_display', height = "100%"), class = "p-2")
          )
        ),

        card(
          full_screen = TRUE,
          style = "height: 50%;",
          card_header(
            "Answer Key"
          ),
          card_body(rHandsontableOutput("stats_display", height = "100%"), class = "p-2")
        )
      )
    )
  )
}
