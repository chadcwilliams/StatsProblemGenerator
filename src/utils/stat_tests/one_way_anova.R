one_way_anova <- function(input, output, stats, plotdata) {
  
  # --------------------------------------------------------------
  # Randomly determine number of groups (3–5)
  # --------------------------------------------------------------
  k <- sample(3:5, 1)
  
  # --------------------------------------------------------------
  # Determine group sizes (equal n per group)
  # --------------------------------------------------------------
  n_per_group <- input$num_of_participants
  N <- n_per_group * k
  
  # --------------------------------------------------------------
  # Create group means
  # --------------------------------------------------------------
  range  <- input$value_range[2] - input$value_range[1]
  center <- input$value_range[1] + range / 2
  
  # Slightly larger separation than before
  offset <- runif(1, range/16, range/10)
  
  # Create base sequence
  base_means <- seq(center - offset,
                    center + offset,
                    length.out = k)
  
  # Small jitter (kept subtle)
  jitter_amount <- runif(k, -range/40, range/40)
  
  group_means <- base_means + jitter_amount
  
  # Keep within bounds
  group_means <- pmin(pmax(group_means,
                           input$value_range[1] + 1),
                      input$value_range[2] - 1)
  
  group_means <- round(group_means, 2)
  
  # --------------------------------------------------------------
  # Generate raw data
  # --------------------------------------------------------------
  raw_data <- list()
  
  for (i in 1:k) {
    temp <- round(rnorm(
      n_per_group,
      mean = group_means[i],
      sd   = range / 6
    ))
    
    temp <- pmin(pmax(temp,
                      input$value_range[1]),
                 input$value_range[2])
    
    raw_data[[i]] <- temp
  }
  
  # --------------------------------------------------------------
  # Prepare data for plotting
  # --------------------------------------------------------------
  plotdata$data <- data.frame(
    Value = unlist(raw_data),
    Group = factor(rep(paste0("Group ", seq_len(k)),
                       each = n_per_group))
  )
  
  # --------------------------------------------------------------
  # Compute group descriptives (n, mean, SS)
  # --------------------------------------------------------------
  n_vec  <- rep(n_per_group, k)
  mean_vec <- rep(NA, k)
  SS_vec   <- rep(NA, k)
  
  for (i in 1:k) {
    mean_vec[i] <- round(mean(raw_data[[i]]), 2)
    SS_vec[i]   <- round(sum((raw_data[[i]] - mean_vec[i])^2), 2)
  }
  
  data <- data.frame(
    Group = paste("Group", 1:k),
    n     = n_vec,
    Mean  = mean_vec,
    SS    = SS_vec
  )
  
  # --------------------------------------------------------------
  # Grand Mean (from group means)
  # --------------------------------------------------------------
  weighted_totals <- round(n_vec * mean_vec, 4)
  grand_mean <- round(sum(weighted_totals) / N, 4)
  
  # --------------------------------------------------------------
  # Sum of Squares Between
  # --------------------------------------------------------------
  SSB_components <- round(n_vec * (mean_vec - grand_mean)^2, 4)
  SS_between <- round(sum(SSB_components), 4)
  
  # --------------------------------------------------------------
  # Sum of Squares Within
  # --------------------------------------------------------------
  SS_within <- round(sum(SS_vec), 4)
  
  # --------------------------------------------------------------
  # Degrees of Freedom
  # --------------------------------------------------------------
  df_between <- k - 1
  df_within  <- N - k
  df_total   <- N - 1
  
  # --------------------------------------------------------------
  # Critical F value (right-tailed)
  # --------------------------------------------------------------
  
  alpha <- 0.05
  
  F_crit <- round(
    qf(alpha,
       df_between,
       df_within,
       lower.tail = FALSE),
    4
  )
  
  # --------------------------------------------------------------
  # Mean Squares
  # --------------------------------------------------------------
  MS_between <- round(SS_between / df_between, 4)
  MS_within  <- round(SS_within / df_within, 4)
  
  # --------------------------------------------------------------
  # F statistic
  # --------------------------------------------------------------
  F_obs <- round(MS_between / MS_within, 4)
  
  # --------------------------------------------------------------
  # p-value
  # --------------------------------------------------------------
  p_obs <- round(pf(F_obs, df_between, df_within,
                    lower.tail = FALSE), 4)
  
  # --------------------------------------------------------------
  # Effect Size (eta squared)
  # --------------------------------------------------------------
  SS_total <- round(SS_between + SS_within, 4)
  eta_sq <- round(SS_between / SS_total, 4)
  
  # --------------------------------------------------------------
  # Cohen's f
  # --------------------------------------------------------------
  cohen_f = round(sqrt(eta_sq / (1 - eta_sq)), 4)
  
  # --------------------------------------------------------------
  # Decision
  # --------------------------------------------------------------
  H0 <- if (p_obs < .05) "Reject" else "Retain"
  H1 <- if (p_obs < .05) "Accept" else "Suspend"
  
  # --------------------------------------------------------------
  # Output table
  # --------------------------------------------------------------
  statistics <- data.frame(
    k             = k,
    n_total       = N,
    F_crit        = F_crit,
    grand_mean    = grand_mean,
    SS_g          = SS_between,
    SS_e          = SS_within,
    df_g          = df_between,
    df_e          = df_within,
    MS_g          = MS_between,
    MS_e          = MS_within,
    F_obs         = F_obs,
    p             = p_obs,
    eta_squared   = eta_sq,
    cohen_f       = cohen_f,
    H0            = H0,
    H1            = H1
  )
  
  # --------------------------------------------------------------
  # Outputs
  # --------------------------------------------------------------
  
  stats$data_table <- statistics
  output$data_display <- renderRHandsontable({
    
    tbl <- as.data.frame(t(data))
    tbl$Statistic <- rownames(tbl)
    rownames(tbl) <- NULL
    
    tbl <- tbl[, c("Statistic", setdiff(names(tbl), "Statistic"))]
    
    rhandsontable(
      tbl,
      rowHeaders = FALSE,
      width = "100%"
    ) %>%
      hot_col("Statistic", readOnly = TRUE) %>%
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