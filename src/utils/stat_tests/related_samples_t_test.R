related_samples_t_test <- function(input, output, stats, plotdata, problemdata) {
  
  # --------------------------------------------------------------
  # Create paired data
  # --------------------------------------------------------------
  
  #Determine different means for each distribution
  range = input$value_range[2] - input$value_range[1]
  center = input$value_range[1] + range / 2
  offset = range / 10
  if (runif(1) < .5) {
    m1 = center + offset
    m2 = center - offset
  } else {
    m1 = center - offset
    m2 = center + offset
  }
  
  #Determine data
  raw <- data.frame(
    Pre = round(rnorm(
      input$num_of_participants,
      mean = m1,
      sd = range / 6
    )),
    Post = round(rnorm(
      input$num_of_participants,
      mean = m2,
      sd = range / 6
    ))
  )
  
  raw$Diff <- raw$Post - raw$Pre
  
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
    Data = raw$Diff
  )
  data$mu[1] = 0
  data$p_alpha = c(.05, rep(NA, input$num_of_participants - 1))
  data$direction = c(direction_label, rep(NA, input$num_of_participants - 1))
  data$n = input$num_of_participants
  data$D_Mean = c(round(mean(data$Data),4), rep(NA, input$num_of_participants - 1))
  data$SS = round(sum(((data$Data - mean(data$Data))^2)),4)
  
  # --------------------------------------------------------------
  # Descriptive statistics
  # --------------------------------------------------------------
  D_bar <- mean(data$Data)
  SD_D  <- sd(data$Data)
  SE_D  <- SD_D / sqrt(input$num_of_participants)
  
  t_obs <- D_bar / SE_D
  df    <- input$num_of_participants - 1
  
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
    `t(crit)`       = t_crit,
    df              = df,
    Mean_Difference = round(D_bar, 4),
    `s<sub>D</sub>` = round(SD_D, 4),
    SE_D            = round(SE_D, 4),
    `t(obs)`        = round(t_obs, 4),
    `p(obs)`        = round(p_obs, 4),
    H0              = H0,
    H1              = H1,
    check.names = FALSE
  )
  names(statistics)[names(statistics) == "p_alpha"] <- "p(\u03b1)"
  names(statistics)[names(statistics) == "Mean_Difference"] <- "D\u0304"
  names(statistics)[names(statistics) == "SE_D"] <- "s<sub>D\u0304</sub>"
  
  # --------------------------------------------------------------
  # Plot data
  # --------------------------------------------------------------
  plotdata$data <- data.frame(
    Pre  = raw$Pre,
    Post = raw$Post,
    Diff = raw$Diff
  )
  
  # --------------------------------------------------------------
  # Outputs
  # --------------------------------------------------------------
  
  stats$data_table <- statistics
  
  tbl <- as.data.frame(t(data[1, 2:ncol(data)]))
  tbl$Variable <- rownames(tbl)
  rownames(tbl) <- NULL
  
  label_map <- c(
    mu     = "\u03bc<sub>D\u0304</sub>",
    p_alpha = "p(\u03b1)",
    D_Mean = "D\u0304",
    SS     = "SS<sub>D</sub>"
  )
  tbl$Variable <- ifelse(
    tbl$Variable %in% names(label_map),
    label_map[tbl$Variable],
    tbl$Variable
  )
  
  tbl <- tbl[, c("Variable", names(tbl)[1])]
  names(tbl)[2] <- "Value"
  
  tbl$Value <- vapply(tbl$Value, function(v) {
    v_trim <- trimws(as.character(v))
    if (grepl("^-?[0-9]+\\.[0-9]+$", v_trim)) {
      sprintf("%.4f", as.numeric(v_trim))
    } else {
      v_trim
    }
  }, character(1))
  
  problemdata$table <- tbl
  problemdata$label_col <- "Variable"

  output$data_display <- renderRHandsontable({
    
    rhandsontable(
      tbl,
      rowHeaders = FALSE,
      width = "100%"
    ) %>%
      hot_col("Variable", readOnly = TRUE, renderer = "
        function(instance, td, row, col, prop, value, cellProperties) {
          td.innerHTML = value;
          return td;
        }
      ") %>%
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