UI = function(){
  fluidPage(tags$head(tags$style(type = "text/css", ".irs {max-width: 946px;}")),
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  "Test_300A",
                  label = "Psyc 300A",
                  choices = c(
                    " " = "",
                    "Frequency Distribution" = 1,
                    "Descriptives" = 2,
                    "Single Participant Z-Test" = 3,
                    "Correlation & Regression" = 4,
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
                    "Power (calculate n)" = 10
                  ),
                  selected = ""
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
}