descriptives = function(input, output, stats, plotdata, problemdata) {         
  #Descriptives
  range_min = input$value_range[1]
  range_max = input$value_range[2]
  n = input$num_of_participants
  
  #Create Data (drawn from a Normal distribution, skewed 20% of the time,
  #rounded to integers, and clipped to stay within the requested value range)
  mu = (range_min + range_max) / 2
  sigma = (range_max - range_min) / 6  # baseline spread if symmetric
  
  skew_prob = 0.33  # 20% chance of a skewed distribution
  is_skewed = runif(1) < skew_prob
  
  if (is_skewed) {
    skew_ratio = runif(1, min = 3, max = 5)  # how strong the skew is
    direction = sample(c(-1, 1), 1)  # -1 = left skew, 1 = right skew
    if (direction == 1) {
      sigma_left = sigma / sqrt(skew_ratio)
      sigma_right = sigma * sqrt(skew_ratio)
    } else {
      sigma_left = sigma * sqrt(skew_ratio)
      sigma_right = sigma / sqrt(skew_ratio)
    }
  } else {
    sigma_left = sigma
    sigma_right = sigma
  }
  
  z = rnorm(n)
  data = mu + ifelse(z >= 0, abs(z) * sigma_right, -abs(z) * sigma_left)
  data = round(data)
  data = pmin(pmax(data, range_min), range_max)  # clip any stray outliers into range
  
  #Force a Mode (By duplicating one of the numbers)
  forced_indices = integer(0)
  if (length(unique(data)) == n) {
    index = sample(1:n, 2)
    data[index[1]] = data[index[2]]
    forced_indices = index
  }
  
  #Force the mean to land on a whole number, or X.50 when possible
  mean_options = if (n %% 2 == 0) c(0, n / 2) else c(0)  # X.50 only achievable when n is even
  target_remainder = sample(mean_options, 1)
  
  current_sum = sum(data)
  remainder = current_sum %% n
  diff = target_remainder - remainder
  if (diff < 0) diff = diff + n
  if (diff > n / 2) diff = diff - n  # take the shorter direction to adjust
  
  adjustable_idx = setdiff(seq_along(data), forced_indices)
  if (length(adjustable_idx) == 0) adjustable_idx = seq_along(data)  # fallback for tiny n
  
  step = sign(diff)
  remaining = abs(diff)
  max_attempts = 20000
  attempts = 0
  while (remaining > 0 && attempts < max_attempts) {
    attempts = attempts + 1
    idx = sample(adjustable_idx, 1)
    new_val = data[idx] + step
    if (new_val >= range_min && new_val <= range_max) {
      data[idx] = new_val
      remaining = remaining - 1
    }
  }
  
  plotdata$data = as.data.frame(data)
  #Setup Mode Function
  mod = function(data) {
    unique_x = unique(data)
    tabulate_x = tabulate(match(data, unique_x))
    unique_x[tabulate_x == max(tabulate_x)]
  }
  #Setup Semi-Interquartile Range Function
  siqr = function(data) {
    sorted_data = sort(data)
    if (length(sorted_data) %% 2 == 0) {
      q1 = median(sorted_data[1:(length(sorted_data) / 2)])
      q3 = median(sorted_data[((length(sorted_data) / 2) +
                                 1):length(sorted_data)])
    } else{
      q1 = median(sorted_data[1:((length(sorted_data) - 1) / 2)])
      q3 = median(sorted_data[((length(sorted_data) - ((
        length(sorted_data) - 1
      ) / 2)) + 1):length(sorted_data)])
    }
    (q3 - q1) / 2
  }
  #Create Stats
  descriptives = data_table = data.frame(
    Mode = mod(data),
    Median = median(data),
    Mean = mean(data),
    Range = range(data)[2] - range(data)[1],
    SIQR = siqr(data),
    MAD = median(abs(data - median(data))),
    SS = sum((data - mean(data)) ^ 2),
    Var = sum((data - mean(data)) ^ 2) / input$num_of_participants,
    SD = sqrt(sum((
      data - mean(data)
    ) ^ 2) / input$num_of_participants),
    SkewP = (3 * (mean(data) - median(data))) / sqrt(sum((
      data - mean(data)
    ) ^ 2) / input$num_of_participants)
  )
  #Clear the Duplicate Values
  if (dim(descriptives)[1] > 1) {
    descriptives[2:dim(descriptives)[1], 2:dim(descriptives)[2]] = NA
  }
  #Round all numeric columns to 4 decimal places max
  descriptives[] = lapply(descriptives, function(col) {
    if (is.numeric(col)) round(col, 4) else col
  })
  #Set outputs
  stats$data_table = descriptives
  
  # Reshape the raw data into a 2D grid for display
  ncols = 10  # adjust to however many columns you want per row
  nrows = ceiling(length(data) / ncols)
  padded = c(data, rep(NA, nrows * ncols - length(data)))
  data_grid = as.data.frame(matrix(padded, nrow = nrows, ncol = ncols, byrow = TRUE))
  colnames(data_grid) = paste0("V", 1:ncols)
  
  problemdata$table <- data_grid
  problemdata$col_headers <- rep("", ncol(data_grid))

  output$data_display = renderRHandsontable(
    rhandsontable(
      data_grid,
      rowHeaders = FALSE,
      colHeaders = rep("", ncol(data_grid)),
      width = "100%",
      useTypes = FALSE
    ) %>%
      hot_table(stretchH = "all") %>%
      hot_context_menu(FALSE) %>%
      hot_cols(readOnly = TRUE)
  )
  
  output$stats_display = renderRHandsontable({
    
  })
  output$distribution_display = renderPlot({
    
  })
}