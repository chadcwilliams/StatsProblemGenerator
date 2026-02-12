power_n <- function(input, output, stats, plotdata) {
  
  # --------------------------------------------------------------
  # Open Power Table
  # --------------------------------------------------------------  
  power_table <- read.csv("materials/power_table_delta.csv", header = TRUE) 
  power_table <- power_table[1:26, ]
  power_alphas <- list(
    twotailed = c(.10, .05, .02, .01, .005, .001),
    onetailed = c(.05, .025, .01, .005, .0025, .0005)
  )
  
  # --------------------------------------------------------------
  # Direction
  # --------------------------------------------------------------
  dir <- runif(1)
  if (dir < .5) {
    direction <- 1
    direction_label <- "Two-Tail"
  } else {
    direction <- 3
    direction_label <- "One-Tail"
  }
  
  # --------------------------------------------------------------
  # Create data
  # --------------------------------------------------------------

  #Determine Design
  alpha_value = .05
  
  design = if (runif(1) < .5) "related samples" else "independent samples"

  #Determine Cohen's d
  d = sample(seq(-1, 1, by = 0.05), 1)
  while (d == 0) {
    d = sample(seq(-1, 1, by = 0.05), 1)
  }
  
  #Determine Power
  alpha_level = if (direction == 1) {
    power_alphas$twotailed
  } else {
    power_alphas$onetailed
  }
  alpha_index = which(abs(alpha_level - alpha_value) < 1e-8)+1
  available_power = power_table[, alpha_index]
  power = sample(available_power, 1)
  power_index = which(available_power == power) 
  
  # --------------------------------------------------------------
  # Store data
  # --------------------------------------------------------------
  
  data = data.frame(
    design = design,
    direction = direction_label,
    alpha = alpha_value,
    power = power,
    d = d
  )
  
  # --------------------------------------------------------------
  # Create Answer key
  # --------------------------------------------------------------
  k = if (design == "related samples") 1 else 2
  delta = power_table$delta[power_index]
  n = round((delta/d)^2, 4)
  n = round(k^2 * n, 4)
    
  statistics = data.frame(
    k = k,
    delta = delta,
    n = ceiling(n)
  )
  
  # --------------------------------------------------------------
  # Outputs
  # --------------------------------------------------------------
  
  stats$data_table <- statistics
  output$data_display <- renderRHandsontable({
    
    tbl <- as.data.frame(t(data))
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
  