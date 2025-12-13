single_sample_z_test = function(input, output, stats, plotdata) {      
    #Single Sample Z-Test
    #Create Data
    data = data.frame(
        Data = sample(
            input$value_range[1]:input$value_range[2],
            input$num_of_participants,
            replace = TRUE
        ),
        n = input$num_of_participants,
        mu = c(
            sample(1:20, 1),
            rep(NA, input$num_of_participants - 1)
        )
    )
    data$mu[1] = (round(rnorm(
        1, mean(data$Data), sd(data$Data) / 5
    )))
    data$sigma = c((round(runif(1, 0.5, 2.5),2)),
                    rep(NA, input$num_of_participants - 1))
    dir = runif(1)
    if (dir < .5) {
        data$direction = c('Two-Tail',
                            rep(NA, input$num_of_participants - 1))
        direction = 1
    } else if (dir < .75) {
        data$direction = c('One-Tail (lower)',
                            rep(NA, input$num_of_participants - 1))
        direction = 2
    }else{
        data$direction = c('One-Tail (higher)',
                            rep(NA, input$num_of_participants - 1))
        direction = 3
    }
    
    data$p_alpha = c(.05, rep(NA, input$num_of_participants - 1))
    data$X_Mean = c(mean(data$Data),
                    rep(NA, input$num_of_participants - 1))
    plotdata$data = as.data.frame(data$Data)
    #Create Stats
    descriptives = data_table = data.frame(
        SE = data$sigma[1] / sqrt(input$num_of_participants),
        z_Obs = (data$X_Mean[1] - data$mu[1]) / (data$sigma[1] /
                                                        sqrt(input$num_of_participants)),
        z_Crit = if (direction == 1) {
            '+-1.96'
        } else if (direction == 2) {
            '-1.645'
        } else{
            '+1.645'
        }
    )
    descriptives$p_obs = pnorm(round((data$X_Mean[1] - data$mu[1]) / (data$sigma[1] / sqrt(input$num_of_participants)),4))
    if (descriptives$z_Obs>=0){descriptives$p_obs=1-descriptives$p_obs}
    if (direction == 1) {descriptives$p_obs=descriptives$p_obs*2}
        
    descriptives$p_alpha = .05
    if (direction == 1){
        descriptives$H0 = if (descriptives$p_obs < .05){'Reject'}else{'Retain'}
        descriptives$H1 = if (descriptives$p_obs < .05){'Accept'}else{'Suspend'}}
    else if (direction == 2){
        if (descriptives$z_Obs<0){
            descriptives$H0 = if (descriptives$p_obs < .05){'Reject'}else{'Retain'}
            descriptives$H1 = if (descriptives$p_obs < .05){'Accept'}else{'Suspend'}}
        else{
            descriptives$H0 = 'Retain'
            descriptives$H1 = 'Suspend'}
    } else {
        if (descriptives$z_Obs>0){
            descriptives$H0 = if (descriptives$p_obs < .05){'Reject'}else{'Retain'}
            descriptives$H1 = if (descriptives$p_obs < .05){'Accept'}else{'Suspend'}}
        else{
            descriptives$H0 = 'Retain'
            descriptives$H1 = 'Suspend'}
    }
    
    
    #Set Outputs
    stats$data_table = descriptives
    output$data_display = renderRHandsontable(rhandsontable(as.data.frame(t(data[1, 2:dim(data)[2]]))))
    output$stats_display = renderRHandsontable({
        
    })
    output$distribution_display = renderPlot({
        
    })
    #### WHAT NEEDS TO BE RETURNED? ####
    return(list(input = input, output = output, stats = stats, plotdata = plotdata))
}