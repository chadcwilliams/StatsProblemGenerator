freq_distribution = function(input, output, stats, plotdata) {    
  #Create Data (drawn from a Normal distribution, skewed 33% of the time,
  #rounded to integers, and clipped to stay within the requested value range)
  range_min = input$value_range[1]
  range_max = input$value_range[2]
  mu = (range_min + range_max) / 2
  sigma = (range_max - range_min) / 6  # baseline spread if symmetric
  
  skew_prob = 0.33  # 33% chance of a skewed distribution
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
  
  z = rnorm(input$num_of_participants)
  data = mu + ifelse(z >= 0, abs(z) * sigma_right, -abs(z) * sigma_left)
  data = round(data)
  data = pmin(pmax(data, range_min), range_max)  # clip any stray outliers into range
  
  plotdata$data = as.data.frame(data)
  #Create Stats
  freq_dist = table(data)
  rel_freq = prop.table(freq_dist)
  
  frequency_distribution = data.frame(
    Data = min(data):max(data),
    Frequency = NA,
    Relative_Frequency = NA,
    Cumulative_Frequency = NA,
    Cum_Rel_Freq = NA
  )
  
  frequency_dist = data.frame(
    Data = rownames(freq_dist),
    Frequency = as.integer(freq_dist),
    Relative_Frequency = as.numeric(rel_freq),
    Cumulative_Frequency = cumsum(freq_dist),
    Cum_Rel_Freq = cumsum(rel_freq)
  )
  for (counter in 1:dim(freq_dist)[1]) {
    frequency_distribution[frequency_distribution$Data == frequency_dist$Data[counter],] = frequency_dist[counter,]
  }
  
  missing = which(is.na(frequency_distribution$Frequency))
  frequency_distribution$Frequency[missing] = 0
  frequency_distribution$Relative_Frequency[missing] = 0.00
  for (counter in 1:dim(as.data.frame(missing))[1]) {
    frequency_distribution$Cumulative_Frequency[missing[counter]] = frequency_distribution$Cumulative_Frequency[missing[counter] -
                                                                                                                  1]
    frequency_distribution$Cum_Rel_Freq[missing[counter]] = frequency_distribution$Cum_Rel_Freq[missing[counter] -
                                                                                                  1]
  }
  frequency_distribution$Frequency = as.integer(frequency_distribution$Frequency)
  frequency_distribution$Relative_Frequency = round(frequency_distribution$Relative_Frequency, 4)
  frequency_distribution$Cum_Rel_Freq = round(frequency_distribution$Cum_Rel_Freq, 4)
  
  #Set outputs
  stats$data_table = frequency_distribution[order(nrow(frequency_distribution):1),]
  
  # Reshape the raw data into a 2D grid for display
  ncols = 10  # adjust to however many columns you want per row
  nrows = ceiling(length(data) / ncols)
  padded = c(data, rep(NA, nrows * ncols - length(data)))
  data_grid = as.data.frame(matrix(padded, nrow = nrows, ncol = ncols, byrow = TRUE))
  colnames(data_grid) = paste0("V", 1:ncols)
  
  output$data_display = renderRHandsontable(
    rhandsontable(
      data_grid,
      rowHeaders = FALSE,
      colHeaders = FALSE,
      width = "100%",
      useTypes = FALSE
    ) %>%
      hot_table(stretchH = "all") %>%
      hot_context_menu(FALSE) %>%
      hot_cols(readOnly = TRUE)
  )
}