####################################################################
####          Written by Chad C. Williams, 2021-2026            ####
####                   www.chadcwilliams.com                    ####
####################################################################

options(scipen = 999) #Remove scientific notation
library(shiny) #Shinydir
library(rsconnect) #Shiny
library(BSDA) #z-test function
library(rhandsontable) #Data tables
library(ggplot2) #Plotting
library(faux) #Creating correlated data (rnorm_multi)
library(rstatix) #Dependency of faux
library(gridExtra) #PDF report layout (tables + plot combined into one PDF)
library(dplyr) #Used by pdf_report.R's plot-building helpers

#Setup
source("utils/ui.R")
source("utils/observe_events.R")
source("utils/pdf_report.R")
source("utils/stat_tests/freq_distribution.R")
source("utils/stat_tests/descriptives.R")
source("utils/stat_tests/correlation_regression.R")
source("utils/stat_tests/z_scores.R")
source("utils/stat_tests/single_participant_z_test.R")
source("utils/stat_tests/single_sample_z_test.R")
source("utils/stat_tests/single_sample_t_test.R")
source("utils/stat_tests/related_samples_t_test.R")
source("utils/stat_tests/correlation_advanced.R")
source("utils/stat_tests/independent_samples_t_test.R")
source("utils/stat_tests/power_n.R")
source("utils/stat_tests/power_power.R")
source("utils/stat_tests/one_way_anova.R")
source("utils/stat_tests/multiple_comparisons.R")
source("utils/stat_tests/factorial_anova.R")
source("utils/stat_tests/chi_goodness.R")
source("utils/stat_tests/chi_homoind.R")

#UI
ui = UI

#Server
server = function(input, output, session) {
    stats = reactiveValues(data_table = NULL)
    plotdata = reactiveValues(data = NULL)

    # Holds the raw "problem" table shown in the Data card, so the PDF
    # download can reuse it. Populated by every test script below.
    problemdata = reactiveValues(table = NULL, col_headers = NULL, label_col = NULL, seed = NULL)

    # Test id -> display name, used for PDF titles/filenames.
    test_labels = list(
      '1' = "Frequency Distribution",
      '2' = "Descriptives",
      '4' = "Correlation & Regression",
      '17' = "Z-scores",
      '3' = "Single Participant Z-Test",
      '5' = "Single Sample Z-Test",
      '6' = "Single Sample T-Test",
      '7' = "Related Samples T-Test",
      '8' = "Correlation (Advanced)",
      '9' = "Independent Samples T-Test",
      '10' = "Power (calculate n)",
      '11' = "Power (calculate power)",
      '12' = "One-Way ANOVA",
      '13' = "Multiple Comparisons",
      '14' = "Multifactorial ANOVA",
      '15' = "Chi-Squared (Goodness of Fit)",
      '16' = "Chi-Squared (Homogeneity & Independence)"
    )

    test_fns = list(
      '1' = freq_distribution,
      '2' = descriptives,
      '4' = correlation_regression,
      '17' = z_scores,
      '3' = single_participant_z_test,
      '5' = single_sample_z_test,
      '6' = single_sample_t_test,
      '7' = related_samples_t_test,
      '8' = correlation_advanced,
      '9' = independent_samples_t_test,
      '10' = power_n,
      '11' = power_power,
      '12' = one_way_anova,
      '13' = multiple_comparisons,
      '14' = factorial_anova,
      '15' = chi_squared_goodness,
      '16' = chi_squared_homoind
    )
    
    active_test <- reactiveVal(NULL)

    # Wipes everything tied to the previously-selected test - the
    # underlying reactiveValues (stats/plotdata/problemdata) and the
    # three rendered outputs (Data, Answer Key, Plot) - so switching
    # tests never leaves stale content or data on screen. Without
    # this, the old test's table/plot/answers stay visible (and the
    # old test's data stays in problemdata/stats/plotdata) until
    # "Generate Data" is clicked, which can produce mismatched or
    # broken-looking output if the person clicks "Show Answers" or
    # "Plot Data" for the new test before regenerating.
    clear_test_state <- function() {
      stats$data_table <- NULL
      plotdata$data <- NULL
      problemdata$table <- NULL
      problemdata$col_headers <- NULL
      problemdata$label_col <- NULL
      problemdata$seed <- NULL

      output$data_display <- renderRHandsontable({ NULL })
      output$stats_display <- renderRHandsontable({ NULL })
      output$distribution_display <- renderPlot({ NULL })
    }

    observeEvent(input$Test_300A, ignoreInit = TRUE, {
      if (input$Test_300A != "") {
        updateSelectInput(session, "Test_300B", selected = "")
        clear_test_state()
        active_test(input$Test_300A)
      }
    })
    
    observeEvent(input$Test_300B, ignoreInit = TRUE, {
      if (input$Test_300B != "") {
        updateSelectInput(session, "Test_300A", selected = "")
        clear_test_state()
        active_test(input$Test_300B)
      }
    })
    
    observeEvent(input$refresh, {
      req(active_test())
      fn = test_fns[[active_test()]]
      req(fn)

      # If the person typed a seed, use it exactly; otherwise pick a
      # random one ourselves (rather than leaving R's RNG unseeded)
      # so there's always a concrete, reproducible value to show and
      # store - this is what makes "recall this problem set later"
      # possible even when nobody set a seed on purpose. Importantly,
      # this is picked fresh from R's ambient (not fixed) RNG state
      # each time, so leaving the box empty still gives a different
      # problem set on every click, not a repeated default.
      seed_input <- trimws(input$seed)
      seed_val <- if (nzchar(seed_input) && grepl("^-?[0-9]+$", seed_input)) {
        as.integer(seed_input)
      } else {
        sample.int(1e6, 1)
      }
      set.seed(seed_val)
      problemdata$seed <- seed_val

      fn(input, output, stats, plotdata, problemdata)
    })

    # Bold "Seed used: ..." line shown right under the seed input, so
    # the person always has the value on screen to write down or
    # re-enter later to reproduce this exact problem set. Always
    # rendered (even before a seed exists) so the layout doesn't shift
    # when "Generate Data" is first clicked - only the number appears.
    output$seed_display <- renderUI({
      seed_text <- if (is.null(problemdata$seed)) "" else problemdata$seed
      tags$div(tags$b(paste0("Seed used: ", seed_text)), class = "mb-2")
    })

    # Only show the Download PDF button once a test has been selected
    # AND a data set has been generated, so the download handler below
    # can never be invoked in a state where there's nothing to
    # download (no silent-error/500 path).
    output$download_pdf_slot <- renderUI({
      req(active_test())
      req(problemdata$table)
      downloadButton('download_pdf', 'Download PDF', icon = icon("download"), class = "btn-outline-secondary")
    })

    output$download_pdf <- downloadHandler(
      filename = function() {
        test_id <- active_test()
        label <- if (!is.null(test_id) && test_id %in% names(test_labels)) {
          gsub("[^A-Za-z0-9]+", "_", test_labels[[test_id]])
        } else {
          "problem_set"
        }
        # Use the seed rather than a timestamp, so the filename itself
        # doubles as the value needed to reproduce this exact problem
        # set later. problemdata$seed should always be set by the time
        # this button is reachable (it's set in the same click that
        # populates problemdata$table, which gates the button), but
        # fall back to a timestamp just in case.
        suffix <- if (!is.null(problemdata$seed)) {
          problemdata$seed
        } else {
          format(Sys.time(), "%Y%m%d_%H%M%S")
        }
        paste0(label, "_", suffix, ".pdf")
      },
      content = function(file) {
        test_id <- active_test()
        pdf_path <- generate_pdf_report(
          problemdata = problemdata,
          stats = stats,
          plotdata = plotdata,
          test_id = as.numeric(test_id),
          test_name = test_labels[[test_id]]
        )
        file.copy(pdf_path, file, overwrite = TRUE)
      }
    )

    observe_events(input, output, stats, plotdata, active_test)
}

shinyApp(ui = ui, server = server)
