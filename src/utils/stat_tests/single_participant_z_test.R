single_participant_z_test = function(input, output, stats, plotdata) {      
  #Single Participant Z-Test
  #Create Data
  data = data.frame(
    x = sample(
      seq(input$value_range[1], input$value_range[2], by = .1),
      1
    ),
    mu = sample(
      seq(input$value_range[1], input$value_range[2], by = .1),
      1
    ),
    sigma = round(rnorm(1, (
      input$value_range[2] - input$value_range[1]
    ) / 5, .1), 2)
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
    `z(obs)` = (data$x - data$mu) / data$sigma,
    `p(x and below)` = round(pnorm(
      round((data$x - data$mu) / data$sigma, digits = 2)
    ), digits = 4),
    `p(x and above)` = round(pnorm(
      round((data$x - data$mu) / data$sigma, digits = 2), lower.tail = F
    ), digits = 4),
    check.names = FALSE
  )
  #Round all numeric columns to 4 decimal places max
  descriptives[] = lapply(descriptives, function(col) {
    if (is.numeric(col)) round(col, 4) else col
  })
  
  #Set Outputs
  stats$data_table = descriptives
  stats$p_value = descriptives$`p(x and below)`  # used by the distribution plot
  output$data_display = renderRHandsontable({
    
    tbl <- as.data.frame(t(data))
    tbl$Variable <- rownames(tbl)
    rownames(tbl) <- NULL
    
    tbl$Variable <- ifelse(tbl$Variable == "mu", "\u03bc",
                           ifelse(tbl$Variable == "sigma", "\u03c3", tbl$Variable))
    
    tbl <- tbl[, c("Variable", setdiff(names(tbl), "Variable"))]
    names(tbl)[2] <- "Value"
    
    tbl$Value <- vapply(tbl$Value, function(v) {
      v_trim <- trimws(as.character(v))
      if (grepl("^-?[0-9]+\\.[0-9]+$", v_trim)) {
        sprintf("%.2f", as.numeric(v_trim))
      } else {
        v_trim
      }
    }, character(1))
    
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