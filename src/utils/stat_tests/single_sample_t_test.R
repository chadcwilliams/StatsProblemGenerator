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
  effect_size = runif(1, 0.10, 0.75)          # distance in SDs
  direction_sign = sample(c(-1, 1), 1)     # push mu up or down randomly
  data$mu[1] = round(mean(data$Data) + direction_sign * effect_size * sd(data$Data))
  
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
  
  plotdata$data = data.frame(
    Group = factor(
      c("Population (\u03bc)", "Sample (x\u0304 & SD)"),
      levels = c("Population (\u03bc)", "Sample (x\u0304 & SD)")
    ),
    Mean = c(data$mu[1], data$X_Mean[1]),
    SD = c(NA, sd(data$Data))
  )
  
  #Create Stats
  t=if (direction == 1){t.test(data$Data,mu=data$mu[1])
  }else if (direction == 2) {t.test(data$Data,mu=data$mu[1],alternative = 'less')
  }else{t.test(data$Data,mu=data$mu[1],alternative = 'greater')}
  
  SS = sum(((data$Data - mean(data$Data))^2))
  s = sd(data$Data)
  SE = sd(data$Data) / sqrt(input$num_of_participants)
  df = input$num_of_participants-1
  t_obs = as.numeric(t[['statistic']])
  t_crit = if (direction == 1) {
    paste('+/-',toString(round(qt(p=.975, df=input$num_of_participants-1),2)))
  } else if (direction == 2) {
    round(qt(p=.05, df=input$num_of_participants-1),2)
  } else{
    round(qt(p=.95, df=input$num_of_participants-1),2)
  }
  
  p_obs = as.numeric(t[['p.value']])
  p_alpha = .05
  if (direction == 1){
    H0 = if (p_obs < .05){'Reject'}else{'Retain'}
    H1 = if (p_obs < .05){'Accept'}else{'Suspend'}}
  else if (direction == 2){
    if (t_obs<0){
      H0 = if (p_obs < .05){'Reject'}else{'Retain'}
      H1 = if (p_obs < .05){'Accept'}else{'Suspend'}}
    else{
      H0 = 'Retain'
      H1 = 'Suspend'}
  } else {
    if (t_obs>0){
      H0 = if (p_obs < .05){'Reject'}else{'Retain'}
      H1 = if (p_obs < .05){'Accept'}else{'Suspend'}}
    else{
      H0 = 'Retain'
      H1 = 'Suspend'}
  }
  
  descriptives = data.frame(
    SS = SS,
    s = s,
    SE = SE,
    df = df,
    `t(obs)` = t_obs,
    `t(crit)` = t_crit,
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