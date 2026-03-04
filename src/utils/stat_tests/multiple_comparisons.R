multiple_comparisons <- function(input, output, stats, plotdata) {
  
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
  # Determine planned comparison information
  # --------------------------------------------------------------
  total_num_comparisons = k*(k-1)/2
  total_planned_comparisons = k-1

  groups <- seq(1, k)
  all_comparisons <- combn(groups, 2, simplify = FALSE)
  planned_comparisons <- split(groups, ceiling(seq_along(groups) / 2))
  planned_comparisons <- sample(all_comparisons, total_planned_comparisons)
  
  # --------------------------------------------------------------
  # Determine post-hoc comparison information
  # --------------------------------------------------------------
  if (runif(1) < 1/3) {
    comparison_type <- "Control (Group 1) vs. Other Groups"
    post_hoc_test = "Dunnett's Test"
  } else {
    comparison_type <- "All Pairwise Comparisons"
    if (k == 3) {
      post_hoc_test = "Fisher's LSD"
    } else {
      post_hoc_test = "Tukey HSD"
    }
  }
  
  #Check for significance 
  if (H0 == "Retain") {
    post_hoc_test <- "None, ANOVA was not significant"
  }
  
  # --------------------------------------------------------------
  # Add info to data table
  # --------------------------------------------------------------
  data$df_e = rep(" ", k)
  data$df_e[[1]] = df_within
  data$MS_e = rep(" ", k)
  data$MS_e[[1]] = MS_within
  data$F = rep(" ", k)
  data$F[[1]] = F_obs
  data$p = rep(" ", k)
  data$p[[1]] = if (p_obs < .05) "< .05" else "> .05"
  pc_labels <- sapply(planned_comparisons, 
                      function(x) paste("Group", x, collapse = " vs. "))
  n <- nrow(data)
  k_pc <- length(pc_labels)
  data$planned_comparisons <- c(pc_labels, rep(" ", n - k_pc))
  data$post_hocs <- rep(" ", n)
  data$post_hocs[[1]] <- comparison_type

  # --------------------------------------------------------------
  # Planned Comparisons
  # --------------------------------------------------------------
  dunns_table <- read.csv("materials/dunns_table.csv", header = TRUE)
  c = paste('comparisons_', total_planned_comparisons, sep = '')
  df_row = which.min(abs(dunns_table$df - df_within))
  dunns_crit <- dunns_table[df_row, c]

  dunns_t = round(2 * MS_within, 4)
  dunns_t = round(dunns_t/n_per_group, 4)
  dunns_t = round(sqrt(dunns_t), 4)
  dunns_t = round(dunns_crit * dunns_t, 4)

  statistics <- data.frame(
    PLANNED_COMPARISONS = "",
    Dunns_t = dunns_t
  )

  for (i in 1:length(planned_comparisons)) {
    comp <- planned_comparisons[[i]]
    group1 <- comp[1]
    group2 <- comp[2]
    mean_diff <- round(abs(mean_vec[group1] - mean_vec[group2]), 4)
    mean_significance = if (abs(mean_diff) > abs(dunns_t)) ", (p < .05)" else ", (p > .05)"
    planned_label = paste0("Group ", group1, " vs. Group ", group2)
    statistics[[planned_label]] <- paste0(mean_diff, mean_significance)
  }
  
  # --------------------------------------------------------------
  # Post-Hoc Comparisons
  # --------------------------------------------------------------
  if (post_hoc_test == "None, ANOVA was not significant") {
    pc_crit <- NA
    pc_t <- NA
    statistics$POST_HOC_COMPARISONS <- ""
    statistics$Post_hoc_test <- post_hoc_test
  } else {
  
    mult = 2
    if (post_hoc_test == "Tukey HSD") {
      mult <- 1
    }
    pc_filename <- switch(
      post_hoc_test,
      "Dunnett's Test" = "dunnett_table.csv",
      "Tukey HSD" = "tukey_table.csv",
      "Fisher's LSD" = "t_table.csv",
      stop("Unknown post hoc test: ", post_hoc_test)
    )
    
    pc_table = read.csv(paste0("materials/", pc_filename), header = TRUE)
    df_row = which.min(abs(pc_table$df - df_within))
    pc_crit <- dunns_table[df_row, k]
    
    pc_t = round(mult * MS_within, 4)
    pc_t = round(pc_t/n_per_group, 4)
    pc_t = round(sqrt(pc_t), 4)
    pc_t = round(pc_crit * pc_t, 4)
    
    print('planned')
    print(k)
    all_pairs <- combn(1:k, 2, simplify = FALSE)
    print('all pairs')
    print(all_pairs)
    pair_key <- function(x) paste(sort(x), collapse = "_")
    all_keys <- sapply(all_pairs, pair_key)
    planned_keys <- sapply(planned_comparisons, pair_key)
    print('planned keys')
    print(planned_keys)
    remaining_pairs <- all_pairs[!all_keys %in% planned_keys]  
    print('remaining')
    print(remaining_pairs)
  
    statistics$POST_HOC_COMPARISONS <- ""
    statistics$Post_hoc_test <- post_hoc_test
    statistics$crit_mean_value <- pc_t
    
    for (i in seq_along(remaining_pairs)) {
      
      comp <- remaining_pairs[[i]]
      group1 <- comp[1]
      group2 <- comp[2]
      
      mean_diff <- round(abs(mean_vec[group1] - mean_vec[group2]), 4)
      
      mean_significance <- if (mean_diff > abs(pc_t)) {
        ", (p < .05)"
      } else {
        ", (p > .05)"
      }
      comparison_label <- paste0("Group ", group1, " vs. Group ", group2)
      statistics[[comparison_label]] <- paste0(mean_diff, mean_significance)
    }
  }
  
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