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
  #Round all numeric columns to 4 decimal places max
  descriptives[] = lapply(descriptives, function(col) {
    if (is.numeric(col)) round(col, 4) else col
  })
  
  #Set Outputs
  stats$data_table = descriptives
  stats$p_value = descriptives$P_Value_of_X_and_Below  # used by the distribution plot
  output$data_display = renderRHandsontable({
    
    tbl <- as.data.frame(t(data))
    tbl$Variable <- rownames(tbl)
    rownames(tbl) <- NULL
    
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