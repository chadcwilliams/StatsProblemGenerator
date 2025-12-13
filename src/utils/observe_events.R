observe_events = function(input, output, stats, plotdata){
  observeEvent(input$answers,
               {
                 output$stats_display = renderRHandsontable(if (input$Test == 3) {
                   rhandsontable(stats$data_table) %>%
                     hot_col("P_Value_of_X_and_Below", format = "0.0000") %>%
                     hot_col("P_Value_of_X_and_Above", format = "0.0000")
                 }else if (input$Test == 5 | input$Test == 6){
                   rhandsontable(stats$data_table) %>%
                     hot_col("p_obs", format = "0.0000")
                 }
                 else{
                   rhandsontable(stats$data_table)
                 })
               })
  
  observeEvent(input$distribution,
               {
                 output$distribution_display = renderPlot(if (input$Test == 3) {
                   ggplot(aes(x = 1:100, y = data), data = plotdata$data) +
                     geom_line() +
                     geom_vline(xintercept = round((
                       stats$data_table$P_Value_of_X_and_Below * 100
                     )) + .5, color = 'red') +
                     theme_void()
                 } else if (input$Test == 4) {
                   ggplot(aes(x = X, y = Y), data = plotdata$data) +
                     geom_point(size = 4, alpha = .5) +
                     geom_smooth(method = lm, se = F) +
                     geom_segment(
                       y = min(plotdata$data$Y),
                       x = (stats$data_table$ax + (
                         stats$data_table$bx * min(plotdata$data$Y)
                       )),
                       yend = max(plotdata$data$Y),
                       xend = (stats$data_table$ax + (
                         stats$data_table$bx * max(plotdata$data$Y)
                       )),
                       color = 'red'
                     ) +
                     theme_classic() +
                     theme(text = element_text(size = 20))
                 }
                 else{
                   ggplot(aes(x = data), data = plotdata$data) +
                     geom_histogram(color = "#E27D60",
                                    fill = "#E8A87C",
                                    binwidth = 1) +
                     scale_x_continuous(
                       breaks = 1:input$value_range[2],
                       limits = c(input$value_range[1] - 1, input$value_range[2] +
                                    1),
                       name = 'X Values'
                     ) +
                     ylab('Frequency Count') +
                     theme_classic()
                 })
               })
}