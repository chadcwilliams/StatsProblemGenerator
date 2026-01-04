####################################################################
####              Written by Chad C. Williams, 2021             ####
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

#Setup
source("utils/ui.R")
source("utils/observe_events.R")
source("utils/stat_tests/freq_distribution.R")
source("utils/stat_tests/descriptives.R")
source("utils/stat_tests/single_participant_z_test.R")
source("utils/stat_tests/correlation_regression.R")
source("utils/stat_tests/single_sample_z_test.R")
source("utils/stat_tests/single_sample_t_test.R")
source("utils/stat_tests/related_samples_t_test.R")
source("utils/stat_tests/correlation_advanced.R")

#UI
ui = UI

#Server
server = function(input, output) {
    stats = reactiveValues(data_table = NULL)
    plotdata = reactiveValues(data = NULL)
    
    test_fns = list(
      '1' = freq_distribution,
      '2' = descriptives,
      '3' = single_participant_z_test,
      '4' = correlation_regression,
      '5' = single_sample_z_test,
      '6' = single_sample_t_test,
      '7' = related_samples_t_test,
      '8' = correlation_advanced
    )
    
    observeEvent(input$refresh, {
      fn = test_fns[[as.character(input$Test)]]
      req(fn)
      fn(input, output, stats, plotdata)
    })

    observe_events(input, output, stats, plotdata)
}

shinyApp(ui = ui, server = server)
