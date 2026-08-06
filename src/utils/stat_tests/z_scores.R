z_scores = function(input, output, stats, plotdata) {      
  #Single Participant Z-Test
  #Create Data
  data = data.frame(
    X = sample(seq(input$value_range[1], input$value_range[2], by = .1), 1),
    X_mean = sample(seq(input$value_range[1], input$value_range[2], by = .1), 1),
    SD = rnorm(1, (input$value_range[2] - input$value_range[1]) / 5, .1)
  )
  data$X = round(data$X, 2)
  data$X_mean = round(data$X_mean, 2)
  data$SD = round(data$SD, 2)
  
  data2 = data.frame(data = dnorm(
    seq((data$X_mean - (4 * data$SD)), (data$X_mean + (4 * data$SD)), length.out = 100),
    mean = data$X_mean,
    sd = data$SD
  ))
  plotdata$data = data2
  
  #Create Table
  z = round((data$X - data$X_mean) / data$SD, 4)
  descriptives = data.frame(
    Z_Score = z
  )
  
  #Set Outputs
  stats$data_table = descriptives
  stats$p_value = round(pnorm(z), digits = 4)  # kept separate, used only for plotting
  output$data_display = renderRHandsontable(rhandsontable(as.data.frame(t(data))))
  output$stats_display = renderRHandsontable({
    
  })
  output$distribution_display = renderPlot({
    
  })
}