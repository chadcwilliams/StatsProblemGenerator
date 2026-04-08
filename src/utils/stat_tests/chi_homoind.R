chi_squared_homoind <- function(input, output, stats, plotdata) {
  
  check_decimals <- function(x) {
    all(abs(x * 100 - round(x * 100)) < 1e-8)
  }
  
  repeat {
    
    # --------------------------------------------------------------
    # Columns (3–6), rows fixed at 2
    # --------------------------------------------------------------
    k <- sample(2:5, 1)
    r <- 2
    
    # --------------------------------------------------------------
    # Total sample size (biased toward clean decimals)
    # --------------------------------------------------------------
    N <- sample(c(100, 150, 200, 250, 300, 350, 400, 450, 500), 1)
    
    # --------------------------------------------------------------
    # Row totals
    # --------------------------------------------------------------
    row1_total <- sample(seq(0.4*N, 0.6*N, by = 1), 1)
    row2_total <- N - row1_total
    
    # --------------------------------------------------------------
    # Column probabilities
    # --------------------------------------------------------------
    probs <- runif(k)
    probs <- probs / sum(probs)
    
    # --------------------------------------------------------------
    # Observed counts
    # --------------------------------------------------------------
    row1 <- as.vector(rmultinom(1, row1_total, probs))
    row2 <- as.vector(rmultinom(1, row2_total, probs))
    
    observed_matrix <- rbind(row1, row2)
    
    # --------------------------------------------------------------
    # Expected values
    # --------------------------------------------------------------
    row_totals <- rowSums(observed_matrix)
    col_totals <- colSums(observed_matrix)
    
    expected <- outer(row_totals, col_totals) / N
    
    # --------------------------------------------------------------
    # Constraints
    # --------------------------------------------------------------
    if (any(expected < 5)) next
    if (any(observed_matrix < 5)) next
    
    # Ensure clean decimals (≤ 2 decimal places naturally)
    if (!check_decimals(expected)) next
    
    break
  }
  
  # --------------------------------------------------------------
  # Chi-square
  # --------------------------------------------------------------
  chi_components <- round((observed_matrix - expected)^2 / expected, 4)
  chi_sq <- round(sum(chi_components), 4)
  
  df <- (r - 1) * (k - 1)
  
  # --------------------------------------------------------------
  # p-value (table lookup)
  # --------------------------------------------------------------
  chi_table <- list(
    "1" = c("0.50"=0.4549,"0.40"=0.7083,"0.30"=1.0742,"0.20"=1.6424,"0.10"=2.7055,"0.05"=3.8415,"0.02"=5.4119,"0.01"=6.6349,"0.005"=7.8794,"0.001"=10.8276),
    "2" = c("0.50"=1.3863,"0.40"=1.8326,"0.30"=2.4079,"0.20"=3.2189,"0.10"=4.6052,"0.05"=5.9915,"0.02"=7.8240,"0.01"=9.2103,"0.005"=10.5966,"0.001"=13.8155),
    "3" = c("0.50"=2.3660,"0.40"=2.9462,"0.30"=3.6649,"0.20"=4.6416,"0.10"=6.2514,"0.05"=7.8147,"0.02"=9.8374,"0.01"=11.3449,"0.005"=12.8382,"0.001"=16.2662),
    "4" = c("0.50"=3.3567,"0.40"=4.0446,"0.30"=4.8784,"0.20"=5.9886,"0.10"=7.7794,"0.05"=9.4877,"0.02"=11.6678,"0.01"=13.2767,"0.005"=14.8603,"0.001"=18.4668),
    "5" = c("0.50"=4.3515,"0.40"=5.1319,"0.30"=6.0644,"0.20"=7.2893,"0.10"=9.2364,"0.05"=11.0705,"0.02"=13.3882,"0.01"=15.0863,"0.005"=16.7496,"0.001"=20.5150),
    "6" = c("0.50"=5.3481,"0.40"=6.2108,"0.30"=7.2311,"0.20"=8.5581,"0.10"=10.6446,"0.05"=12.5916,"0.02"=15.0332,"0.01"=16.8119,"0.005"=18.5476,"0.001"=22.4577)
  )
  
  crit_vals <- chi_table[[as.character(df)]]
  p_levels <- as.numeric(names(crit_vals))
  
  ord <- order(crit_vals)
  crit_vals <- crit_vals[ord]
  p_levels <- p_levels[ord]
  
  if (chi_sq < crit_vals[1]) {
    p_display <- "p > .50"
  } else if (chi_sq >= tail(crit_vals, 1)) {
    p_display <- "p < .001"
  } else {
    for (i in 1:(length(crit_vals) - 1)) {
      if (chi_sq >= crit_vals[i] && chi_sq < crit_vals[i + 1]) {
        p_high <- p_levels[i]
        p_low  <- p_levels[i + 1]
        
        if (p_high > .05) {
          p_display <- paste0("p > ", format(p_low, nsmall = 3))
        } else {
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
  # DATA TABLE (Observed only)
  # --------------------------------------------------------------
  data_wide <- data.frame(
    Group = c("Group 1", "Group 2"),
    observed_matrix,
    stringsAsFactors = FALSE
  )
  
  colnames(data_wide)[-1] <- col_names
  
  # --------------------------------------------------------------
  # FORMATTED MATRIX (O (E))
  # --------------------------------------------------------------
  formatted_matrix <- matrix(
    paste0(
      observed_matrix,
      " (",
      format(expected, nsmall = 2),
      ")"
    ),
    nrow = 2
  )
  
  # --------------------------------------------------------------
  # Marginal sums
  # --------------------------------------------------------------
  row_totals <- rowSums(observed_matrix)
  col_totals <- colSums(observed_matrix)
  
  # --------------------------------------------------------------
  # Add row totals (as last column)
  # --------------------------------------------------------------
  formatted_with_row_totals <- cbind(
    formatted_matrix,
    as.character(row_totals)
  )
  
  # --------------------------------------------------------------
  # Column totals row (plus grand total)
  # --------------------------------------------------------------
  col_totals_row <- c(as.character(col_totals), as.character(sum(row_totals)))
  
  # --------------------------------------------------------------
  # Build full table
  # --------------------------------------------------------------
  answer_table <- data.frame(
    Statistic = c("Group 1", "Group 2", "Column Sums", "Chi-square", "p-value"),
    rbind(
      formatted_with_row_totals,
      col_totals_row,
      c(as.character(chi_sq), rep("", k)),
      c(p_display, rep("", k))
    ),
    stringsAsFactors = FALSE
  )
  
  # --------------------------------------------------------------
  # Column names (add Total column)
  # --------------------------------------------------------------
  colnames(answer_table)[-1] <- c(col_names, "Row Sums")
  
  stats$data_table <- answer_table
  
  # --------------------------------------------------------------
  # PLOT DATA
  # --------------------------------------------------------------
  plotdata$data <- data.frame(
    Category = rep(col_names, each = 2),
    Group = factor(rep(c("Group 1", "Group 2"), times = k)),
    Count = as.vector(t(observed_matrix))
  )
  
  # --------------------------------------------------------------
  # OUTPUTS
  # --------------------------------------------------------------
  output$data_display <- renderRHandsontable({
    rhandsontable(data_wide, rowHeaders = FALSE, width = "100%") %>%
      hot_table(stretchH = "all") %>%
      hot_cols(readOnly = TRUE)
  })
  
  output$stats_display <- renderRHandsontable({
    rhandsontable(answer_table, rowHeaders = FALSE, width = "100%") %>%
      hot_table(stretchH = "all") %>%
      hot_cols(readOnly = TRUE)
  })
  
  output$distribution_display <- renderPlot({
    ggplot(plotdata$data,
           aes(x = Category, y = Count, fill = Group)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_classic()
  })
}