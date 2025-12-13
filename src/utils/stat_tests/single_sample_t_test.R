single_sample_t_test = function(input, output, stats, plotdata) {      
    #Single Sample t-Test
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
        1, mean(data$Data), sd(data$Data) / 2.5
    )))
    data$SS = sum(((data$Data - mean(data$Data))^2))
    dir = runif(1)
    if (dir < .5) {
        data$direction = c('Two-Tail',
                            rep(NA, input$num_of_participants - 1))
        direction = 1
    } else if (dir < .75) {
        data$direction = c('One-Tail (lower)',
                            rep(NA, input$num_of_participants - 1))
        direction = 2
    }
    else{
        data$direction = c('One-Tail (higher)',
                            rep(NA, input$num_of_participants - 1))
        direction = 3
    }
    
    data$p_alpha = c(.05, rep(NA, input$num_of_participants - 1))
    data$X_Mean = c(mean(data$Data),
                    rep(NA, input$num_of_participants - 1))
    plotdata$data = as.data.frame(data$Data)
    #Create Stats
    t=if (direction == 1){t.test(data$Data,mu=data$mu[1])
    }else if (direction == 2) {t.test(data$Data,mu=data$mu[1],alternative = 'less')
            }else{t.test(data$Data,mu=data$mu[1],alternative = 'greater')}
    descriptives = data_table = data.frame(
        SS = sum(((data$Data - mean(data$Data))^2)),
        s = sd(data$Data),
        SE = sd(data$Data) / sqrt(input$num_of_participants),
        df = input$num_of_participants-1,
        t_Obs = as.numeric(t[['statistic']]),
        t_Crit = 
        if (direction == 1) {
            paste('+-',toString(round(qt(p=.975, df=input$num_of_participants-1),2)))
        } else if (direction == 2) {
            round(qt(p=.05, df=input$num_of_participants-1),2)
        } else{
            round(qt(p=.95, df=input$num_of_participants-1),2)
        }
    )
    
    descriptives$p_obs = as.numeric(t[['p.value']])
    descriptives$p_alpha = .05
    if (direction == 1){
        descriptives$H0 = if (descriptives$p_obs < .05){'Reject'}else{'Retain'}
        descriptives$H1 = if (descriptives$p_obs < .05){'Accept'}else{'Suspend'}}
    else if (direction == 2){
        if (descriptives$t_Obs<0){
            descriptives$H0 = if (descriptives$p_obs < .05){'Reject'}else{'Retain'}
            descriptives$H1 = if (descriptives$p_obs < .05){'Accept'}else{'Suspend'}}
        else{
            descriptives$H0 = 'Retain'
            descriptives$H1 = 'Suspend'}
    } else {
        if (descriptives$t_Obs>0){
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
}