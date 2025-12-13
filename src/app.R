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
library(here) #Project paths

#Setup
source(here("src", "utils", "stat_tests", "freq_distribution.R"))
source(here("src", "utils", "stat_tests", "descriptives.R"))
source(here("src", "utils", "stat_tests", "single_participant_z_test.R"))
source(here("src", "utils", "stat_tests", "correlation_regression.R"))
source(here("src", "utils", "stat_tests", "single_sample_z_test.R"))
source(here("src", "utils", "stat_tests", "single_sample_t_test.R"))

#UI
ui = fluidPage(tags$head(tags$style(type = "text/css", ".irs {max-width: 946px;}")),
               sidebarLayout(
                   sidebarPanel(
                       selectInput(
                           "Test",
                           label = " ",
                           choices = list(
                               "Frequency Distribution" = 1,
                               "Descriptives" = 2,
                               "Correlation & Regression" = 4,
                               "Single Participant Z-Test" = 3,
                               "Single Sample Z-Test" = 5,
                               "Single Sample T-Test" = 6
                           ),
                           selected = 1
                       ),
                       sliderInput(
                           inputId = 'num_of_participants',
                           label = 'Number of Participants',
                           value = 10,
                           min = 2,
                           max = 100,
                           step = 1,
                           width = '95%'
                       ),
                       sliderInput(
                           inputId = 'value_range',
                           label = 'Range of Values',
                           value = c(1, 10),
                           min = 1,
                           max = 100,
                           step = 1,
                           width = '95%'
                       ),
                       actionButton('refresh', 'Refresh'),
                       actionButton('distribution', 'Plot Data'),
                       actionButton('answers', 'Show Answers')
                   ),
                   mainPanel(fluidRow(
                       column(6, rHandsontableOutput("data_display")),
                       column(6, plotOutput('distribution_display'))
                   ),
                   fluidRow(column(
                       12, rHandsontableOutput("stats_display")
                   )))
               ))

#Server
server = function(input, output) {
    stats = reactiveValues(data_table = NULL)
    plotdata = reactiveValues(data = NULL)
    observeEvent(input$refresh, {
        if (input$Test == 1) {
            return_list = freq_distribution(input, output, stats, plotdata)
        }
        else if (input$Test == 2) {
            return_list = descriptives(input, output, stats, plotdata)
        }
        else if (input$Test == 3) {
            return_list = single_participant_z_test(input, output, stats, plotdata)
        }
        else if (input$Test == 4) {
            return_list = correlation_regression(input, output, stats, plotdata)
        }
        else if (input$Test == 5) {
            return_list = single_sample_z_test(input, output, stats, plotdata)
        }
        else {
            return_list = single_sample_t_test(input, output, stats, plotdata)
        }
        input = return_list$input
        output = return_list$output
        stats = return_list$stats
        plotdata = return_list$plotdata
    })

    observeEvent(input$answers,
                 {
                     output$stats_display = renderRHandsontable(if (input$Test == 3) {
                         rhandsontable(stats$data_table) %>%
                             hot_col("P_Value_of_X_and_Below", format = "0.0000") %>%
                             hot_col("P_Value_of_X_and_Above", format = "0.0000")
                     }else if (input$Test == 5 | input$Test == 6){
                         rhandsontable(stats$data_table) %>%
                             hot_col("p_obs", format = "0.0000")
                     }
                     else{
                         rhandsontable(stats$data_table)
                     })
                 })

    observeEvent(input$distribution,
                 {
                     output$distribution_display = renderPlot(if (input$Test == 3) {
                         ggplot(aes(x = 1:100, y = data), data = plotdata$data) +
                             geom_line() +
                             geom_vline(xintercept = round((
                                 stats$data_table$P_Value_of_X_and_Below * 100
                             )) + .5, color = 'red') +
                             theme_void()
                     } else if (input$Test == 4) {
                         ggplot(aes(x = X, y = Y), data = plotdata$data) +
                             geom_point(size = 4, alpha = .5) +
                             geom_smooth(method = lm, se = F) +
                             geom_segment(
                                 y = min(plotdata$data$Y),
                                 x = (stats$data_table$ax + (
                                     stats$data_table$bx * min(plotdata$data$Y)
                                 )),
                                 yend = max(plotdata$data$Y),
                                 xend = (stats$data_table$ax + (
                                     stats$data_table$bx * max(plotdata$data$Y)
                                 )),
                                 color = 'red'
                             ) +
                             theme_classic() +
                             theme(text = element_text(size = 20))
                     }
                     else{
                         ggplot(aes(x = data), data = plotdata$data) +
                             geom_histogram(color = "#E27D60",
                                            fill = "#E8A87C",
                                            binwidth = 1) +
                             scale_x_continuous(
                                 breaks = 1:input$value_range[2],
                                 limits = c(input$value_range[1] - 1, input$value_range[2] +
                                                1),
                                 name = 'X Values'
                             ) +
                             ylab('Frequency Count') +
                             theme_classic()
                     })
                 })
}

shinyApp(ui = ui, server = server)
