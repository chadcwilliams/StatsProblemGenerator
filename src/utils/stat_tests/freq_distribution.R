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
    output$data_display = renderRHandsontable(rhandsontable(as.data.frame(data)))
    output$stats_display = renderRHandsontable({
        
    })
    output$distribution_display = renderPlot({
        
    })
    
    #### WHAT NEEDS TO BE RETURNED? ####
    return(list(input = input, output = output, stats = stats, plotdata = plotdata))
}
