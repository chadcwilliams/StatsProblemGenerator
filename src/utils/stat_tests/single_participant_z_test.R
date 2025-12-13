single_participant_z_test = function(input, output, stats, plotdata) {      
    #Single Participant Z-Test
    #Create Data
    data = data.frame(
        X = sample(
            seq(input$value_range[1], input$value_range[2], by = .1),
            1
        ),
        mu = sample(
            seq(input$value_range[1], input$value_range[2], by = .1),
            1
        ),
        sigma = rnorm(1, (
            input$value_range[2] - input$value_range[1]
        ) / 5, .1)
    )
    data2 = data.frame(data = dnorm(
        seq((data$mu - (
            4 * data$sigma
        )), (data$mu + (
            4 * data$sigma
        )), length.out = 100),
        mean = data$mu,
        sd = data$sigma
    ))
    plotdata$data = data2
    
    #Create Table
    descriptives = data.frame(
        Z_Value = (data$X - data$mu) / data$sigma,
        P_Value_of_X_and_Below = round(pnorm(
            round((data$X - data$mu) / data$sigma, digits = 2)
        ), digits = 4),
        P_Value_of_X_and_Above = round(pnorm(
            round((data$X - data$mu) / data$sigma, digits = 2), lower.tail = F
        ), digits = 4)
    )
    #Set Outputs
    stats$data_table = descriptives
    output$data_display = renderRHandsontable(rhandsontable(as.data.frame(t(data))))
    output$stats_display = renderRHandsontable({
        
    })
    output$distribution_display = renderPlot({
        
    })

    #### WHAT NEEDS TO BE RETURNED? ####
    return(list(input = input, output = output, stats = stats, plotdata = plotdata))
}
