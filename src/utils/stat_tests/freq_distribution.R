freq_distribution = function(input, output, stats, plotdata) {    
    #Create Data
    data = sample(
        input$value_range[1]:input$value_range[2],
        input$num_of_participants,
        replace = TRUE
    )
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