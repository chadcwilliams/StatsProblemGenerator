z_scores = function(input, output, stats, plotdata) {      
  #Single Participant Z-Test
  #Create Data
  data = data.frame(
    x = sample(seq(input$value_range[1], input$value_range[2], by = .1), 1),
    X_mean = sample(seq(input$value_range[1], input$value_range[2], by = .1), 1),
    SD = rnorm(1, (input$value_range[2] - input$value_range[1]) / 5, .1)
  )
  data$x = round(data$x, 2)
  data$X_mean = round(data$X_mean, 2)
  data$SD = round(data$SD, 2)
  
  data2 = data.frame(data = dnorm(
    seq((data$X_mean - (4 * data$SD)), (data$X_mean + (4 * data$SD)), length.out = 100),
    mean = data$X_mean,
    sd = data$SD
  ))
  plotdata$data = data2
  
  #Create Table
  z = round((data$x - data$X_mean) / data$SD, 4)
  descriptives = data.frame(
    z = z
  )
  
  #Set Outputs
  stats$data_table = descriptives
  stats$p_value = round(pnorm(z), digits = 4)  # kept separate, used only for plotting
  output$data_display = renderRHandsontable({
    
    tbl <- as.data.frame(t(data))
    tbl$Variable <- rownames(tbl)
    rownames(tbl) <- NULL
    
    tbl$Variable <- ifelse(tbl$Variable == "X_mean", "x\u0304", tbl$Variable)
    
    tbl <- tbl[, c("Variable", setdiff(names(tbl), "Variable"))]
    names(tbl)[2] <- "Value"
    
    rhandsontable(
      tbl,
      rowHeaders = FALSE,
      colHeaders = c("Variable", "Value"),
      width = "100%"
    ) %>%
      hot_col(1, readOnly = TRUE) %>%
      hot_table(
        stretchH = "all",
        highlightRow = TRUE
      ) %>%
      hot_cols(halign = "htLeft", valign = "htMiddle")
  })
  output$stats_display = renderRHandsontable({
    
  })
  output$distribution_display = renderPlot({
    
  })
}