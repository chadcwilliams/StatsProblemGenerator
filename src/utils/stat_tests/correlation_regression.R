correlation_regression = function(input, output, stats, plotdata) {
    #Correlation & Regression
    #Create Data
    data = rnorm_multi(
        n = input$num_of_participants,
        mu = c(
            sample(input$value_range[1]:input$value_range[2], 1),
            sample(input$value_range[1]:input$value_range[2], 1)
        ),
        sd = c(rnorm(
            1, (input$value_range[2] - input$value_range[1]) / 5, .1
        ), rnorm(
            1, (input$value_range[2] - input$value_range[1]) / 5, .1
        )),
        r = sample(seq(-1, 1, .01), 1),
        varnames = c('X', 'Y')
    )
    data$X = as.integer(data$X)
    data$Y = as.integer(data$Y)
    data$X_SD = c(sqrt(sum((
        data$X - mean(data$X)
    ) ^ 2) / dim(data)[1]), rep(NA, dim(data)[1] - 1))
    data$Y_SD = c(sqrt(sum((
        data$Y - mean(data$Y)
    ) ^ 2) / dim(data)[1]), rep(NA, dim(data)[1] - 1))
    plotdata$data = data
    
    #Create Table
    descriptives = data.frame(
        X_Mean = mean(data$X),
        X_SD = sqrt(sum((
            data$X - mean(data$X)
        ) ^ 2) / dim(data)[1]),
        Y_Mean = mean(data$Y),
        Y_SD = sqrt(sum((
            data$Y - mean(data$Y)
        ) ^ 2) / dim(data)[1]),
        SP = sum((data$X - mean(data$X)) * (data$Y - mean(data$Y))),
        COV = sum((data$X - mean(data$X)) * (data$Y - mean(data$Y))) /
            dim(data)[1],
        r = cor(data$X, data$Y)
    )
    descriptives$by = descriptives$r * (descriptives$Y_SD / descriptives$X_SD)
    descriptives$ay = descriptives$Y_Mean - (round(descriptives$by, 4) *
                                                    descriptives$X_Mean)
    descriptives$bx = descriptives$r * (descriptives$X_SD / descriptives$Y_SD)
    descriptives$ax = descriptives$X_Mean - (round(descriptives$bx, 4) *
                                                    descriptives$Y_Mean)
    descriptives$SD_XPrime = round((round(descriptives$X_SD, 2) *
                                        (round(
                                            sqrt(1 - (abs(
                                                round(cor(data$X, data$Y), 4)
                                            ) ^ 2)), 4
                                        ))), 2)
    descriptives$SD_Yprime = round((round(descriptives$Y_SD, 2) *
                                        (round(
                                            sqrt(1 - (abs(
                                                round(cor(data$X, data$Y), 4)
                                            ) ^ 2)), 4
                                        ))), 2)
    
    #Set Outputs
    stats$data_table = descriptives
    output$data_display = renderRHandsontable(rhandsontable(as.data.frame(data)))
    output$stats_display = renderRHandsontable({
        
    })
    output$distribution_display = renderPlot({
        
    })
}
