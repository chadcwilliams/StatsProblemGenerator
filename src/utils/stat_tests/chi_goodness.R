chi_squared_goodness <- function(input, output, stats, plotdata) {
  
  repeat {
    
    # --------------------------------------------------------------
    # Number of categories (3–6)
    # --------------------------------------------------------------
    k <- sample(3:6, 1)
    
    # --------------------------------------------------------------
    # Total sample size
    # --------------------------------------------------------------
    N <- sample(seq(50, 300, by = 50), 1)
    
    # --------------------------------------------------------------
    # Choose null type
    # --------------------------------------------------------------
    null_type <- sample(c("uniform", "proportional"), 1)
    
    # --------------------------------------------------------------
    # Generate expected values
    # --------------------------------------------------------------
    if (null_type == "uniform") {
      
      if (N %% k != 0) next
      
      expected <- rep(N / k, k)
      null_row <- rep(NA, k)
      null_label <- "Null: Uniform"
      
    } else {
      
      repeat {
        percents <- sample(c(5,10,15,20,25,30,35,40,50), k, replace = TRUE)
        percents <- round(percents / sum(percents) * 100)
        if (sum(percents) == 100) break
      }
      
      expected <- (percents / 100) * N
      
      # Fix rounding drift AFTER computing expected
      diff <- N - sum(expected)
      expected[1] <- expected[1] + diff
      
      null_row <- paste0(percents, "%")
      null_label <- "Null: Proportional"
    }
    
    # --------------------------------------------------------------
    # Constraint: Expected ≥ 5
    # --------------------------------------------------------------
    if (any(expected < 5)) next
    
    # --------------------------------------------------------------
    # Generate observed
    # --------------------------------------------------------------
    observed <- rpois(k, lambda = expected)
    
    diff_obs <- N - sum(observed)
    observed[1] <- observed[1] + diff_obs
    
    # Constraint: Observed ≥ 7
    if (any(observed < 7)) next
    
    break
  }
  
  # --------------------------------------------------------------
  # Chi-square
  # --------------------------------------------------------------
  chi_components <- round((observed - expected)^2 / expected, 4)
  chi_sq <- round(sum(chi_components), 4)
  
  df <- k - 1
  
  # --------------------------------------------------------------
  # p-value (chi-square table lookup, corrected + clean)
  # --------------------------------------------------------------
  
  chi_table <- list(
    "1" = c("0.50"=0.4549, "0.40"=0.7083, "0.30"=1.0742, "0.20"=1.6424, "0.10"=2.7055, "0.05"=3.8415, "0.02"=5.4119, "0.01"=6.6349, "0.005"=7.8794, "0.001"=10.8276),
    "2" = c("0.50"=1.3863, "0.40"=1.8326, "0.30"=2.4079, "0.20"=3.2189, "0.10"=4.6052, "0.05"=5.9915, "0.02"=7.8240, "0.01"=9.2103, "0.005"=10.5966, "0.001"=13.8155),
    "3" = c("0.50"=2.3660, "0.40"=2.9462, "0.30"=3.6649, "0.20"=4.6416, "0.10"=6.2514, "0.05"=7.8147, "0.02"=9.8374, "0.01"=11.3449, "0.005"=12.8382, "0.001"=16.2662),
    "4" = c("0.50"=3.3567, "0.40"=4.0446, "0.30"=4.8784, "0.20"=5.9886, "0.10"=7.7794, "0.05"=9.4877, "0.02"=11.6678, "0.01"=13.2767, "0.005"=14.8603, "0.001"=18.4668),
    "5" = c("0.50"=4.3515, "0.40"=5.1319, "0.30"=6.0644, "0.20"=7.2893, "0.10"=9.2364, "0.05"=11.0705, "0.02"=13.3882, "0.01"=15.0863, "0.005"=16.7496, "0.001"=20.5150),
    "6" = c("0.50"=5.3481, "0.40"=6.2108, "0.30"=7.2311, "0.20"=8.5581, "0.10"=10.6446, "0.05"=12.5916, "0.02"=15.0332, "0.01"=16.8119, "0.005"=18.5476, "0.001"=22.4577)
  )
  
  crit_vals <- chi_table[[as.character(df)]]
  p_levels <- as.numeric(names(crit_vals))
  
  # Sort by increasing chi-square
  ord <- order(crit_vals)
  crit_vals <- crit_vals[ord]
  p_levels <- p_levels[ord]
  
  # --------------------------------------------------------------
  # Determine p-value
  # --------------------------------------------------------------
  
  # Case 1: smaller than smallest chi-square
  if (chi_sq < crit_vals[1]) {
    
    p_display <- "p > .50"
    
    # Case 2: larger than largest chi-square
  } else if (chi_sq >= tail(crit_vals, 1)) {
    
    p_display <- "p < .001"
    
  } else {
    
    for (i in 1:(length(crit_vals) - 1)) {
      
      chi_low  <- crit_vals[i]
      chi_high <- crit_vals[i + 1]
      
      if (chi_sq >= chi_low && chi_sq < chi_high) {
        
        p_high <- p_levels[i]       # larger p (e.g., .10)
        p_low  <- p_levels[i + 1]   # smaller p (e.g., .05)
        
        if (p_high > .05) {
          
          # non-significant region
          p_display <- paste0("p > ", format(p_low, nsmall = 3))
          
        } else {
          
          # significant region
          p_display <- paste0("p < ", format(p_high, nsmall = 3))
        }
        break
      }
    }
  }
  
  # --------------------------------------------------------------
  # Column names
  # --------------------------------------------------------------
  col_names <- paste("Category", 1:k)
  
  # --------------------------------------------------------------
  # DATA TABLE (students see this)
  # --------------------------------------------------------------
  # --------------------------------------------------------------
  # DATA TABLE (students see this)
  # --------------------------------------------------------------
  
  observed_display <- as.integer(observed)
  
  # Keep null row clean
  if (null_type == "uniform") {
    null_display <- rep("", k)   # empty instead of NA → prevents coercion issues
  } else {
    null_display <- null_row     # already like "25%"
  }
  
  data_wide <- data.frame(
    Statistic = c("Observed", null_label),
    rbind(observed_display, null_display),
    stringsAsFactors = FALSE
  )
  
  colnames(data_wide)[-1] <- col_names
  
  # --------------------------------------------------------------
  # PLOT DATA (Observed only)
  # --------------------------------------------------------------
  plotdata$data <- data.frame(
    Category = factor(col_names),
    Observed = observed
  )
  
  # --------------------------------------------------------------
  # ANSWER TABLE
  # --------------------------------------------------------------
  answer_table <- data.frame(
    Statistic = c("Observed", "Expected", "Chi-square", "p-value"),
    rbind(
      observed,
      round(expected, 4),
      c(chi_sq, rep(NA, k - 1)),
      c(p_display, rep(NA, k - 1))
    ),
    stringsAsFactors = FALSE
  )
  
  colnames(answer_table)[-1] <- col_names
  
  stats$data_table <- answer_table
  
  # --------------------------------------------------------------
  # OUTPUTS
  # --------------------------------------------------------------
  
  output$data_display <- renderRHandsontable({
    
    rhandsontable(
      data_wide,
      rowHeaders = FALSE,
      width = "100%"
    ) %>%
      hot_col("Statistic", readOnly = TRUE) %>%
      hot_table(stretchH = "all")
    
  })
  
  output$stats_display <- renderRHandsontable({
    
    rhandsontable(
      answer_table,
      rowHeaders = FALSE,
      width = "100%"
    ) %>%
      hot_col("Statistic", readOnly = TRUE) %>%
      hot_table(stretchH = "all")
    
  })
  
  output$distribution_display <- renderPlot({
  })
  
}