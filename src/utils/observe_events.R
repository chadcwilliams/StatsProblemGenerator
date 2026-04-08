observe_events = function(input, output, stats, plotdata, active_test){
  
  observeEvent(input$answers,
               {
                 output$stats_display <- renderRHandsontable({
                   
                   if (active_test() == 14) {
                     
                     tbl <- stats$data_table
                     
                     ht <- rhandsontable(
                       tbl,
                       rowHeaders = FALSE,
                       width = "100%",
                       useTypes = FALSE
                     ) %>%
                       hot_table(stretchH = "all", highlightRow = TRUE) %>%
                       hot_context_menu(FALSE) %>%
                       hot_cols(readOnly = TRUE) %>%
                       hot_col("SS", format = "0.0000") %>%
                       hot_col("MS", format = "0.0000") %>%
                       hot_col("F", format = "0.0000") %>%
                       hot_col("p", format = "0.0000")
                     
                   } else if (active_test() == 15 || active_test() == 16) {
                     
                     # Chi-square (GOF + Homogeneity/Independence)
                     tbl <- stats$data_table
                     
                     ht <- rhandsontable(
                       tbl,
                       rowHeaders = FALSE,
                       width = "100%",
                       useTypes = FALSE
                     ) %>%
                       hot_table(stretchH = "all", highlightRow = TRUE) %>%
                       hot_context_menu(FALSE) %>%
                       hot_cols(readOnly = TRUE)
                     
                   } else {
                     
                     tbl <- as.data.frame(t(stats$data_table[1, ]))
                     tbl$Statistic <- rownames(tbl)
                     rownames(tbl) <- NULL
                     tbl <- tbl[, c("Statistic", names(tbl)[1])]
                     names(tbl)[2] <- "Value"
                     
                     ht <- rhandsontable(
                       tbl,
                       rowHeaders = FALSE,
                       width = "100%",
                       useTypes = FALSE
                     ) %>%
                       hot_table(stretchH = "all", highlightRow = TRUE) %>%
                       hot_context_menu(FALSE) %>%
                       hot_cols(readOnly = TRUE)
                     
                     if (active_test() == 3) {
                       ht <- ht %>% hot_col("Value", format = "0.0000")
                     } else if (active_test() %in% c(5, 6, 7)) {
                       ht <- ht %>% hot_col("Value", format = "0.0000")
                     }
                   }
                   
                   ht
                 })
               })
  
  observeEvent(input$distribution,
               {
                 output$distribution_display = renderPlot(
                   
                   if (active_test() == 3) {
                     
                     ggplot(aes(x = 1:100, y = data), data = plotdata$data) +
                       geom_line() +
                       geom_vline(xintercept = round((
                         stats$data_table$P_Value_of_X_and_Below * 100
                       )) + .5, color = 'red') +
                       theme_void()
                     
                   } else if (active_test() == 4 || active_test() == 8) {
                     
                     ggplot(aes(x = X, y = Y), data = plotdata$data) +
                       geom_point(size = 4, alpha = .5) +
                       (
                         if (active_test() == 4)
                           list(
                             geom_segment(
                               y = min(plotdata$data$Y),
                               x = stats$data_table$ax +
                                 stats$data_table$bx * min(plotdata$data$Y),
                               yend = max(plotdata$data$Y),
                               xend = stats$data_table$ax +
                                 stats$data_table$bx * max(plotdata$data$Y),
                               color = "red"
                             ),
                             geom_smooth(method = lm, se = FALSE)
                           )
                         else NULL
                       ) +
                       theme_classic() +
                       theme(text = element_text(size = 20))
                     
                   } else if (active_test() == 9) {
                     
                     rng <- range(
                       c(plotdata$data$Data1, plotdata$data$Data2),
                       na.rm = TRUE
                     )
                     
                     ggplot(plotdata$data) +
                       geom_histogram(aes(x = Data1),
                                      fill = "#E27D60", alpha = 0.5, binwidth = 1) +
                       geom_histogram(aes(x = Data2),
                                      fill = "#85DCB0", alpha = 0.5, binwidth = 1) +
                       scale_x_continuous(
                         breaks = floor(rng[1]) : ceiling(rng[2]),
                         limits = c(floor(rng[1]) - 1, ceiling(rng[2]) + 1)
                       ) +
                       ylab("Frequency Count") +
                       theme_classic()
                     
                   } else if (active_test() == 12 || active_test() == 13) {
                     
                     rng <- range(plotdata$data$Value, na.rm = TRUE)
                     
                     ggplot(plotdata$data,
                            aes(x = Value, fill = Group, colour = Group)) +
                       geom_density(alpha = 0.3) +
                       scale_x_continuous(
                         breaks = floor(rng[1]) : ceiling(rng[2]),
                         limits = c(floor(rng[1]) - 1,
                                    ceiling(rng[2]) + 1)
                       ) +
                       ylab("Density") +
                       theme_classic()
                     
                   } else if (active_test() == 14) {
                     
                     summary_data <- plotdata$data %>%
                       dplyr::group_by(A, B) %>%
                       dplyr::summarise(Mean = mean(Value), .groups = "drop")
                     
                     ggplot(summary_data, aes(x = A, y = Mean, fill = B)) +
                       geom_bar(stat = "identity",
                                position = position_dodge(width = 0.8)) +
                       theme_classic()
                     
                   } else if (active_test() == 15) {
                     
                     ggplot(plotdata$data, aes(x = Category)) +
                       geom_bar(aes(y = Observed),
                                stat = "identity",
                                fill = "#E27D60") +
                       theme_classic()
                     
                   } else if (active_test() == 16) {
                     
                     # NEW: Chi-square homogeneity / independence
                     ggplot(plotdata$data,
                            aes(x = Category, y = Count, fill = Group)) +
                       
                       geom_bar(
                         stat = "identity",
                         position = position_dodge(width = 0.8),
                         width = 0.7,
                         color = "black"
                       ) +
                       
                       scale_fill_manual(
                         values = c("#E27D60", "#85DCB0")
                       ) +
                       
                       labs(
                         x = "Category",
                         y = "Frequency",
                         fill = "Group"
                       ) +
                       
                       theme_classic() +
                       theme(text = element_text(size = 18))
                     
                   } else {
                     
                     ggplot(aes(x = data), data = plotdata$data) +
                       geom_histogram(color = "#E27D60",
                                      fill = "#E8A87C",
                                      binwidth = 1) +
                       theme_classic()
                   }
                 )
               })
  
  observeEvent(input$refresh, {
    output$stats_display <- renderRHandsontable({ NULL })
  })
}