factorial_anova <- function(input, output, stats, plotdata) {
  
  # --------------------------------------------------------------
  # Factors
  # --------------------------------------------------------------
  k <- sample(3:4, 1)
  b_levels <- 2
  
  # --------------------------------------------------------------
  # Sample size
  # --------------------------------------------------------------
  n_per_group <- input$num_of_participants
  N <- n_per_group * k * b_levels
  
  # --------------------------------------------------------------
  # Create means
  # --------------------------------------------------------------
  range  <- input$value_range[2] - input$value_range[1]
  center <- input$value_range[1] + range / 2
  
  offset <- runif(1, range/16, range/10)
  
  A_means <- seq(center - offset,
                 center + offset,
                 length.out = k)
  
  B_effect <- runif(1, range/20, range/12)
  B_offsets <- c(-B_effect/2, B_effect/2)
  
  interaction_jitter <- matrix(
    runif(k * b_levels, -range/40, range/40),
    nrow = k
  )
  
  # --------------------------------------------------------------
  # Generate raw data
  # --------------------------------------------------------------
  raw_data <- list()
  
  index <- 1
  for (i in 1:k) {
    for (j in 1:b_levels) {
      
      cell_mean <- A_means[i] + B_offsets[j] + interaction_jitter[i, j]
      
      temp <- round(rnorm(
        n_per_group,
        mean = cell_mean,
        sd   = range / 6
      ))
      
      temp <- pmin(pmax(temp,
                        input$value_range[1]),
                   input$value_range[2])
      
      raw_data[[index]] <- temp
      index <- index + 1
    }
  }
  
  # --------------------------------------------------------------
  # Data for plotting
  # --------------------------------------------------------------
  plotdata$data <- data.frame(
    Value = unlist(raw_data),
    A = factor(rep(paste0("A", seq_len(k)),
                   each = b_levels * n_per_group)),
    B = factor(rep(rep(c("B1", "B2"),
                       each = n_per_group),
                   times = k))
  )
  
  # --------------------------------------------------------------
  # Force grand mean to be a whole number (robust version)
  # --------------------------------------------------------------
  
  values <- plotdata$data$Value
  N <- length(values)
  
  current_sum <- sum(values)
  
  target_mean <- round(current_sum / N)
  target_sum  <- target_mean * N
  
  diff <- target_sum - current_sum  # how much we need to fix
  
  # Distribute the adjustment across multiple values safely
  i <- 1
  
  while (diff != 0 && i <= N) {
    
    if (diff > 0) {
      # try to increase value
      if (values[i] < input$value_range[2]) {
        values[i] <- values[i] + 1
        diff <- diff - 1
      }
      
    } else {
      # try to decrease value
      if (values[i] > input$value_range[1]) {
        values[i] <- values[i] - 1
        diff <- diff + 1
      }
    }
    
    i <- i + 1
    
    # loop back if needed
    if (i > N && diff != 0) i <- 1
  }
  
  # Update dataset
  plotdata$data$Value <- values
  
  # Rebuild raw_data to stay consistent
  raw_data <- split(plotdata$data$Value,
                    interaction(plotdata$data$A, plotdata$data$B))
  raw_data <- lapply(raw_data, as.numeric)
  
  # --------------------------------------------------------------
  # 2 × k Cell Summary Table
  # --------------------------------------------------------------
  cell_matrix <- matrix(NA, nrow = b_levels, ncol = k)
  
  for (i in 1:k) {
    for (j in 1:b_levels) {
      
      index <- (i - 1) * b_levels + j
      
      m  <- round(mean(raw_data[[index]]), 2)
      ss <- round(sum((raw_data[[index]] - m)^2), 2)
      
      cell_matrix[j, i] <- paste0(
        "n = ", n_per_group,
        ", x̄ = ", m,
        ", SS = ", ss
      )
    }
  }
  
  data <- as.data.frame(cell_matrix)
  rownames(data) <- paste0("B", 1:b_levels)
  colnames(data) <- paste0("A", 1:k)
  
  # --------------------------------------------------------------
  # Means
  # --------------------------------------------------------------
  # --------------------------------------------------------------
  # Means (rounded at each step)
  # --------------------------------------------------------------
  
  cell_means_emp <- matrix(NA, nrow = k, ncol = b_levels)
  
  for (i in 1:k) {
    for (j in 1:b_levels) {
      index <- (i - 1) * b_levels + j
      cell_means_emp[i, j] <- round(mean(raw_data[[index]]), 4)
    }
  }
  
  # Marginal means
  A_means_emp <- round(rowMeans(cell_means_emp), 4)
  B_means_emp <- round(colMeans(cell_means_emp), 4)
  
  # Grand mean
  grand_mean <- round(mean(cell_means_emp), 4)
  
  # --------------------------------------------------------------
  # SS Total
  # --------------------------------------------------------------
  
  SS_total <- 0
  for (i in 1:k) {
    for (j in 1:b_levels) {
      index <- (i - 1) * b_levels + j
      
      SS_total <- SS_total + round(
        sum((raw_data[[index]] - grand_mean)^2),
        4
      )
    }
  }
  SS_total <- round(SS_total, 4)
  
  # --------------------------------------------------------------
  # SS A
  # --------------------------------------------------------------
  
  SS_A <- round(sum(
    round(b_levels * n_per_group *
            (A_means_emp - grand_mean)^2, 4)
  ), 4)
  
  # --------------------------------------------------------------
  # SS B
  # --------------------------------------------------------------
  
  SS_B <- round(sum(
    round(k * n_per_group *
            (B_means_emp - grand_mean)^2, 4)
  ), 4)
  
  # --------------------------------------------------------------
  # SS Interaction
  # --------------------------------------------------------------
  
  SS_AB <- 0
  for (i in 1:k) {
    for (j in 1:b_levels) {
      
      term <- round(
        (cell_means_emp[i, j] -
           A_means_emp[i] -
           B_means_emp[j] +
           grand_mean)^2,
        4
      )
      
      SS_AB <- SS_AB + round(n_per_group * term, 4)
    }
  }
  SS_AB <- round(SS_AB, 4)
  
  # --------------------------------------------------------------
  # SS Within
  # --------------------------------------------------------------
  
  SS_within <- 0
  for (i in 1:(k * b_levels)) {
    m <- round(mean(raw_data[[i]]), 4)
    
    SS_within <- SS_within + round(
      sum((raw_data[[i]] - m)^2),
      4
    )
  }
  SS_within <- round(SS_within, 4)
  # --------------------------------------------------------------
  # Sum of Squares
  # --------------------------------------------------------------
  SS_total <- sum((plotdata$data$Value - grand_mean)^2)
  
  SS_A <- 0
  for (i in 1:k) {
    SS_A <- SS_A + b_levels * n_per_group *
      (mean(cell_means_emp[i, ]) - grand_mean)^2
  }
  
  SS_B <- 0
  for (j in 1:b_levels) {
    SS_B <- SS_B + k * n_per_group *
      (mean(cell_means_emp[, j]) - grand_mean)^2
  }
  
  SS_AB <- 0
  for (i in 1:k) {
    for (j in 1:b_levels) {
      SS_AB <- SS_AB + n_per_group *
        (cell_means_emp[i, j] -
           A_means_emp[i] -
           B_means_emp[j] +
           grand_mean)^2
    }
  }
  
  SS_within <- SS_total - SS_A - SS_B - SS_AB
  
  # --------------------------------------------------------------
  # Degrees of Freedom
  # --------------------------------------------------------------
  df_A  <- k - 1
  df_B  <- 1
  df_AB <- (k - 1)
  df_E  <- N - (k * b_levels)
  
  # --------------------------------------------------------------
  # Mean Squares
  # --------------------------------------------------------------
  MS_A  <- SS_A / df_A
  MS_B  <- SS_B / df_B
  MS_AB <- SS_AB / df_AB
  MS_E  <- SS_within / df_E
  
  # --------------------------------------------------------------
  # F and p
  # --------------------------------------------------------------
  F_A  <- MS_A / MS_E
  F_B  <- MS_B / MS_E
  F_AB <- MS_AB / MS_E
  
  p_A  <- pf(F_A, df_A, df_E, lower.tail = FALSE)
  p_B  <- pf(F_B, df_B, df_E, lower.tail = FALSE)
  p_AB <- pf(F_AB, df_AB, df_E, lower.tail = FALSE)
  
  format_p <- function(p) {
    if (is.na(p)) return(NA)
    if (p < .05) return("< .05") else return("> .05")
  }
  
  p_A  <- format_p(p_A)
  p_B  <- format_p(p_B)
  p_AB <- format_p(p_AB)
  
  # --------------------------------------------------------------
  # Effect Sizes
  # --------------------------------------------------------------
  SS_total <- SS_A + SS_B + SS_AB + SS_within
  
  eta_A  <- SS_A  / SS_total
  eta_B  <- SS_B  / SS_total
  eta_AB <- SS_AB / SS_total
  
  peta_A  <- SS_A  / (SS_A  + SS_within)
  peta_B  <- SS_B  / (SS_B  + SS_within)
  peta_AB <- SS_AB / (SS_AB + SS_within)
  
  # --------------------------------------------------------------
  # ANOVA Table
  # --------------------------------------------------------------
  SS_cells <- SS_A + SS_B + SS_AB
  df_cells <- (k * b_levels) - 1
  
  SS_total <- SS_cells + SS_within
  df_total <- N - 1
  
  statistics <- data.frame(
    Source = c(
      "Between-Subjects (Cells)",
      "Column (Factor A)",
      "Row (Factor B)",
      "Interaction",
      "Within-Subjects (Error)",
      "Total"
    ),
    SS = round(c(
      SS_cells,
      SS_A,
      SS_B,
      SS_AB,
      SS_within,
      SS_total
    ), 4),
    df = c(
      df_cells,
      df_A,
      df_B,
      df_AB,
      df_E,
      df_total
    ),
    MS = c(
      round(SS_cells / df_cells, 4),
      round(MS_A, 4),
      round(MS_B, 4),
      round(MS_AB, 4),
      round(MS_E, 4),
      NA
    ),
    F = c(
      NA,
      round(F_A, 4),
      round(F_B, 4),
      round(F_AB, 4),
      NA,
      NA
    ),
    p = c(
      NA,
      p_A,
      p_B,
      p_AB,
      NA,
      NA
    ),
    eta2 = c(
      NA,
      round(eta_A, 4),
      round(eta_B, 4),
      round(eta_AB, 4),
      NA,
      NA
    ),
    R2 = c(
      NA,
      round(peta_A, 4),
      round(peta_B, 4),
      round(peta_AB, 4),
      NA,
      NA
    )
  )
  
  # --------------------------------------------------------------
  # Outputs
  # --------------------------------------------------------------
  stats$data_table <- statistics
  
  output$data_display <- renderRHandsontable({
    
    tbl <- data
    
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
  
  output$distribution_display <- renderPlot({
    
  })
}