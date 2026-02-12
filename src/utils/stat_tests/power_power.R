power_power <- function(input, output, stats, plotdata) {
  
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
  # Basic Setup
  # --------------------------------------------------------------
  
  alpha_value = .05
  design = if (runif(1) < .5) "related samples" else "independent samples"
  n = input$num_of_participants   # per group
  
  range  = input$value_range[2] - input$value_range[1]
  center = input$value_range[1] + range / 2
  sd_base = range / 6
  
  N_total = if (design == "related samples") n else 2*n
  k = if (design == "related samples") 1 else 2
  
  # --------------------------------------------------------------
  # Choose delta from table (1.0–3.40)
  # --------------------------------------------------------------
  
  delta <- sample(power_table$delta, 1)
  if (runif(1) < .5) delta <- -delta
  
  # Solve backward for target d
  d_target <- (delta * k) / sqrt(N_total)
  
  # --------------------------------------------------------------
  # Generate Data Until 2-decimal Delta Match
  # --------------------------------------------------------------
  
  match_delta <- FALSE
  attempts <- 0
  
  while (!match_delta && attempts < 10000) {
    
    attempts <- attempts + 1
    
    if (design == "related samples") {
      
      mean_diff_target <- d_target * sd_base
      
      m1 <- center - mean_diff_target/2
      m2 <- center + mean_diff_target/2
      
      raw <- data.frame(
        Pre  = round(rnorm(n, mean = m1, sd = sd_base)),
        Post = round(rnorm(n, mean = m2, sd = sd_base))
      )
      
      raw$Pre  <- pmin(pmax(raw$Pre,  input$value_range[1]), input$value_range[2])
      raw$Post <- pmin(pmax(raw$Post, input$value_range[1]), input$value_range[2])
      
      raw$Diff <- raw$Post - raw$Pre
      
      mean_pre  = round(mean(raw$Pre),2)
      mean_post = round(mean(raw$Post),2)
      sd_diff   = round(sd(raw$Diff),2)
      
      d <- round((mean_post - mean_pre) / sd_diff,4)
      
    } else {
      
      mean_diff_target <- d_target * sd_base
      
      m1 <- center + mean_diff_target/2
      m2 <- center - mean_diff_target/2
      
      raw <- data.frame(
        group1 = round(rnorm(n, mean = m1, sd = sd_base)),
        group2 = round(rnorm(n, mean = m2, sd = sd_base))
      )
      
      raw$group1 <- pmin(pmax(raw$group1, input$value_range[1]), input$value_range[2])
      raw$group2 <- pmin(pmax(raw$group2, input$value_range[1]), input$value_range[2])
      
      mean1 = round(mean(raw$group1),2)
      mean2 = round(mean(raw$group2),2)
      sd1   = sd(raw$group1)
      sd2   = sd(raw$group2)
      
      sp = sqrt(
        ((n - 1)*sd1^2 +
           (n - 1)*sd2^2) /
          (2*n - 2)
      )
      sp = round(sp,2)
      
      d <- round((mean1 - mean2) / sp,4)
    }
    
    delta_computed = round(N_total / k,4)
    delta_computed = round(sqrt(delta_computed),4)
    delta_computed <- round(d * delta_computed,4)
    
    match_delta <- round(abs(delta_computed), 2) ==
      round(abs(delta), 2)
  }

  d <- round(d, 4)
  delta_computed <- round(delta_computed, 4)
  
  # --------------------------------------------------------------
  # Prepare Student-Facing Data
  # --------------------------------------------------------------
  
  if (design == "related samples") {
    
    data = data.frame(
      design = design,
      direction = direction_label,
      alpha = alpha_value,
      n = n,
      mean_diff = round(mean_post - mean_pre, 4),
      sd_diff = round(sd_diff,4)
    )
    
  } else {
    
    data = data.frame(
      design = design,
      direction = direction_label,
      alpha = alpha_value,
      n_k = n,
      n_t = N_total,
      mean1 = round(mean1,2),
      mean2 = round(mean2,2),
      sp = round(sp,2)
    )
  }
  
  # --------------------------------------------------------------
  # Determine Power
  # --------------------------------------------------------------
  
  alpha_level = if (direction == 1) {
    power_alphas$twotailed
  } else {
    power_alphas$onetailed
  }
  
  alpha_index = which(abs(alpha_level - alpha_value) < 1e-8)+1
  
  delta_values = power_table$delta
  delta_index = which.min(abs(delta_values - abs(delta_computed)))
  
  power = power_table[delta_index, alpha_index]
  
  # --------------------------------------------------------------
  # Answer Key
  # --------------------------------------------------------------
  
  statistics = data.frame(
    k = k,
    d = d,
    delta = round(delta_computed,2),
    power = power
  )
  
  # --------------------------------------------------------------
  # Outputs
  # --------------------------------------------------------------
  
  stats$data_table <- statistics
  plotdata$raw <- raw
  
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
  
  output$stats_display <- renderRHandsontable({})
  
  output$distribution_display <- renderPlot({})
}
