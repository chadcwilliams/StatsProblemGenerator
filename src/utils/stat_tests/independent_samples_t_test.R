independent_samples_t_test <- function(input, output, stats, plotdata) {
  
  # --------------------------------------------------------------
  # Create paired data
  # --------------------------------------------------------------
  # Determine different means for each group
  range  <- input$value_range[2] - input$value_range[1]
  center <- input$value_range[1] + range / 2
  offset <- range / 10
  
  if (runif(1) < .5) {
    m1 <- center + offset
    m2 <- center - offset
  } else {
    m1 <- center - offset
    m2 <- center + offset
  }
  
  # Generate data with group difference
  raw <- data.frame(
    group1 = round(rnorm(
      input$num_of_participants,
      mean = m1,
      sd   = range / 6
    )),
    group2 = round(rnorm(
      input$num_of_participants,
      mean = m2,
      sd   = range / 6
    ))
  )
  
  # Enforce value range bounds
  raw$group1 <- pmin(pmax(raw$group1, input$value_range[1]), input$value_range[2])
  raw$group2 <- pmin(pmax(raw$group2, input$value_range[1]), input$value_range[2])

  # --------------------------------------------------------------
  # Direction
  # --------------------------------------------------------------
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
  
  # --------------------------------------------------------------
  # Create data
  # --------------------------------------------------------------
  
  data = data.frame(
    Data1 = raw$group1,
    Data2 = raw$group2
  )
  data$mu[1] = 0
  data$p_alpha = c(.05, rep(NA, input$num_of_participants - 1))
  data$direction = c(direction_label, rep(NA, input$num_of_participants - 1))
  
  data$n1 = input$num_of_participants
  data$n2 = input$num_of_participants
  data$Mean1 = c(round(mean(data$Data1),4),
                  rep(NA, input$num_of_participants - 1))
  data$Mean2 = c(round(mean(data$Data2),4),
                 rep(NA, input$num_of_participants - 1)) 
  data$SS1 = round(sum(((data$Data1 - mean(data$Data1))^2)),4)
  data$SS2 = round(sum(((data$Data2 - mean(data$Data2))^2)),4)
  
  plotdata$data <- data.frame(
    Data1 = data$Data1,
    Data2 = data$Data2
  )
  
  # --------------------------------------------------------------
  # Descriptive statistics
  # --------------------------------------------------------------
  x_bar_1 <- mean(data$Data1)
  x_bar_2 <- mean(data$Data2)
  
  var_p <- (data$SS1[1] + data$SS2[1]) / (input$num_of_participants*2 - 2)
  SE_p  <- sqrt((var_p/input$num_of_participants) + (var_p/input$num_of_participants))
  
  t_obs <- (x_bar_1-x_bar_2) / SE_p
  df    <- input$num_of_participants*2 - 2
  
  # --------------------------------------------------------------
  # Critical value
  # --------------------------------------------------------------
  t_crit <- if (direction == 1) {
    paste0("±", round(qt(.975, df), 4))
  } else {
    round(qt(.95, df), 4)
  }
  
  # --------------------------------------------------------------
  # p-value
  # --------------------------------------------------------------
  p_obs <- pt(abs(t_obs), df, lower.tail = FALSE)
  if (direction == 1) p_obs <- p_obs * 2
  
  #If the t-obs is in the wrong direction, make p; 1-p
  if (direction == 2 && t_obs > 0) p_obs <- 1 - p_obs
  if (direction == 3 && t_obs < 0) p_obs <- 1 - p_obs
  
  # --------------------------------------------------------------
  # Hypothesis decision
  # --------------------------------------------------------------
  H0 <- if (p_obs < .05) "Reject" else "Retain"
  H1 <- if (p_obs < .05) "Accept" else "Suspend"
  
  # --------------------------------------------------------------
  # Output table
  # --------------------------------------------------------------
  statistics <- data.frame(
    Direction       = direction_label,
    p_alpha         = .05,
    t_Crit          = t_crit,
    df              = df,
    Mean_1          = round(x_bar_1, 4),
    Mean_2          = round(x_bar_2, 4),
    SS_1            = round(data$SS1, 4),
    SS_2            = round(data$SS2, 4),
    pooled_variance = round(var_p, 4),
    SE_pooled       = round(SE_p, 4),
    t_obs           = round(t_obs, 4),
    p_obs           = round(p_obs, 4),
    r_squared       = round((t_obs^2) / (t_obs^2 + df), 4),
    H0              = H0,
    H1              = H1
  )
  
  # --------------------------------------------------------------
  # Outputs
  # --------------------------------------------------------------
  
  stats$data_table <- statistics
  output$data_display <- renderRHandsontable({
    
    tbl <- as.data.frame(t(data[1, 3:ncol(data)]))
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
  
  
  output$stats_display <- renderRHandsontable({
    
  })
  output$distribution_display <- renderPlot({
    
  })
}
