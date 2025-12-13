descriptives = function(input, output, stats, plotdata) {         
    #Descriptives
    #Create Data
    data = sample(
        input$value_range[1]:input$value_range[2],
        input$num_of_participants,
        replace = TRUE
    )
    plotdata$data = as.data.frame(data)
    #Force a Mode (By duplicating one of the numbers)
    if (length(unique(data)) == input$num_of_participants) {
        index = sample(1:input$num_of_participants, 2)
        data[index[1]] = data[index[2]]
    }
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
    #Set outputs
    stats$data_table = descriptives
    output$data_display = renderRHandsontable(rhandsontable(as.data.frame(data)))
    output$stats_display = renderRHandsontable({
        
    })
    output$distribution_display = renderPlot({
        
    })
}