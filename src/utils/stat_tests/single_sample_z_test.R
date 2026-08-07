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
  
  effect_size = runif(1, 0.10, 0.75)          # distance in SDs
  direction_sign = sample(c(-1, 1), 1)     # push mu up or down randomly
  data$mu[1] = round(mean(data$Data) + direction_sign * effect_size * sd(data$Data), 1)
  
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
  data$X_Mean = c(round(mean(data$Data), 1),
                  rep(NA, input$num_of_participants - 1))
  
  plotdata$data = data.frame(
    Group = factor(
      c("Population (\u03bc & \u03c3)", "Sample (x\u0304)"),
      levels = c("Population (\u03bc & \u03c3)", "Sample (x\u0304)")
    ),
    Mean = c(data$mu[1], data$X_Mean[1]),
    SD = c(data$sigma[1], NA)
  )
  
  #Create Stats
  SE = data$sigma[1] / sqrt(input$num_of_participants)
  z_obs = (data$X_Mean[1] - data$mu[1]) / (data$sigma[1] /
                                             sqrt(input$num_of_participants))
  z_crit = if (direction == 1) {
    '+-1.96'
  } else if (direction == 2) {
    '-1.645'
  } else{
    '+1.645'
  }
  
  p_obs = if (direction == 1) {
    2 * pnorm(-abs(z_obs))
  } else if (direction == 2) {
    pnorm(z_obs)
  } else {
    pnorm(z_obs, lower.tail = FALSE)
  }
  
  p_alpha = .05
  if (direction == 1){
    H0 = if (p_obs < .05){'Reject'}else{'Retain'}
    H1 = if (p_obs < .05){'Accept'}else{'Suspend'}}
  else if (direction == 2){
    if (z_obs<0){
      H0 = if (p_obs < .05){'Reject'}else{'Retain'}
      H1 = if (p_obs < .05){'Accept'}else{'Suspend'}}
    else{
      H0 = 'Retain'
      H1 = 'Suspend'}
  } else {
    if (z_obs>0){
      H0 = if (p_obs < .05){'Reject'}else{'Retain'}
      H1 = if (p_obs < .05){'Accept'}else{'Suspend'}}
    else{
      H0 = 'Retain'
      H1 = 'Suspend'}
  }
  
  descriptives = data.frame(
    SE = SE,
    `z(obs)` = z_obs,
    `z(crit)` = z_crit,
    `p(obs)` = p_obs,
    p_alpha = p_alpha,
    H0 = H0,
    H1 = H1,
    check.names = FALSE
  )
  names(descriptives)[names(descriptives) == "p_alpha"] <- "p(\u03b1)"
  
  #Round all numeric columns to 4 decimal places max
  descriptives[] = lapply(descriptives, function(col) {
    if (is.numeric(col)) round(col, 4) else col
  })
  
  #Set Outputs
  stats$data_table = descriptives
  output$data_display = renderRHandsontable({
    
    tbl <- as.data.frame(t(data[1, 2:dim(data)[2]]))
    tbl$Variable <- rownames(tbl)
    rownames(tbl) <- NULL
    
    label_map <- c(
      mu     = "\u03bc",
      sigma  = "\u03c3",
      p_alpha = "p(\u03b1)",
      X_Mean = "x\u0304"
    )
    tbl$Variable <- ifelse(
      tbl$Variable %in% names(label_map),
      label_map[tbl$Variable],
      tbl$Variable
    )
    
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