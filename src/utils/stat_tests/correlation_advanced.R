correlation_advanced = function(input, output, stats, plotdata) {
  #Correlation (with p-values and variability explained)
  
  #Create Data
  raw_data = rnorm_multi(
    n = input$num_of_participants,
    mu = c(
      sample(input$value_range[1]:input$value_range[2], 1),
      sample(input$value_range[1]:input$value_range[2], 1)
    ),
    sd = c(rnorm(
      1, (input$value_range[2] - input$value_range[1]) / 5, .1
    ), rnorm(
      1, (input$value_range[2] - input$value_range[1]) / 5, .1
    )),
    r = sample(seq(-1, 1, .01), 1),
    varnames = c('X', 'Y')
  )
  raw_data$X = as.integer(raw_data$X)
  raw_data$Y = as.integer(raw_data$Y)
  plotdata$data = raw_data[, c("X", "Y")]
  print('HERE')
  print(plotdata$data)
  
  #Create direction
  dir <- runif(1)
  if (dir < .5) {
    direction <- 1
    direction_label <- "Two-Tail"
  } else if (dir < .75) {
    direction <- 2
    direction_label <- "One-Tail (lower)"
  } else {
    direction <- 3
    direction_label <- "One-Tail (higher)"
  }
  
  #Create data table
  data = data.frame(
    rho = 0,
    p_alpha = 0.05,
    direction = direction_label,
    n = input$num_of_participants,
    r = cor(raw_data$X, raw_data$Y)
  )
  
  #round to 4 dcimals
  data[] <- lapply(data, function(x) {
    if (is.numeric(x)) round(x, 4) else x
  })
  
  #critical values
  df = input$num_of_participants - 2
  t_crit <- if (direction == 1) {
    paste0("±", round(qt(.975, df), 4))
  } else {
    round(qt(.95, df), 4)
  }
  
  t_crit_val = as.numeric(sub("±", "", t_crit))
  r_crit <- if (direction == 1) {
    paste0("±", round(t_crit_val / sqrt(t_crit_val^2 + df), 4))
  } else {
    round(t_crit_val / sqrt(t_crit_val^2 + df), 4)
  }
  
  #Create Table
  statistics = data.frame(
    n = input$num_of_participants,
    df = input$num_of_participants - 2,
    r = cor(raw_data$X, raw_data$Y)
  )
  statistics$r_crit = r_crit
  statistics$t_crit = t_crit
  statistics$se_r = sqrt((1 - (statistics$r ^ 2)) / (statistics$n - 2))
  statistics$t_obs = statistics$r / statistics$se_r
  statistics$p_obs = pt(abs(statistics$t_obs), df, lower.tail = FALSE)
  if (direction == 1) statistics$p_obs <- statistics$p_obs * 2
  statistics$r_squared = statistics$r ^ 2
  
  H0 <- if (statistics$p_obs < .05) "Reject" else "Retain"
  H1 <- if (statistics$p_obs < .05) "Accept" else "Suspend"
  statistics$H0 = H0
  statistics$H1 = H1
  
  statistics[] <- lapply(statistics, function(x) {
    if (is.numeric(x)) round(x, 4) else x
  })

  #Set Outputs
  stats$data_table = statistics
  
  output$data_display <- renderRHandsontable({
    
    tbl <- as.data.frame(t(data[1, ]))
    tbl$Statistic <- rownames(tbl)
    rownames(tbl) <- NULL
    tbl <- tbl[, c("Statistic", names(tbl)[1])]
    names(tbl)[2] <- "Value"
    
    rhandsontable(
      tbl,
      rowHeaders = FALSE,
      width = "100%"
    ) %>%
      hot_col("Statistic", readOnly = TRUE) %>%
      hot_col("Value", format = "0.000") %>%
      hot_table(
        stretchH = "all",
        highlightRow = TRUE
      )
  })
  
  output$stats_display = renderRHandsontable({
    
  })
  output$distribution_display = renderPlot({
    
  })
}
