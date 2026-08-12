observe_events = function(input, output, stats, plotdata, active_test){
  
  # Restores trailing zeros lost when a mixed-type row gets coerced to
  # character during transpose (e.g. "1.567" -> "1.5670"). Only touches
  # strings that already contain a decimal point, so whole-number fields
  # like df/n/k are left untouched.
  pad_decimals <- function(x, digits = 4) {
    vapply(x, function(v) {
      v_trim <- trimws(as.character(v))
      if (grepl("^-?[0-9]+\\.[0-9]+$", v_trim)) {
        sprintf(paste0("%.", digits, "f"), as.numeric(v_trim))
      } else {
        v_trim
      }
    }, character(1))
  }
  
  # --------------------------------------------------------------
  # Shared pale colour palette used across every plot below.
  # --------------------------------------------------------------
  pal_two   <- c("#F4B8A2", "#A8E0C4")                                   # 2-group fills (coral / mint)
  pal_multi <- c("#F4B8A2", "#A8E0C4", "#A8C4F0", "#F0DFA0", "#CBB6EA")  # up to 5 groups
  pal_accent <- "#C97B6A"                                                # reference lines / fit lines
  
  observeEvent(input$answers,
               {
                 output$stats_display <- renderRHandsontable({
                   
                   if (active_test() == 14) {
                     
                     tbl <- stats$data_table
                     
                     # useTypes = FALSE below renders every column as plain
                     # text, so hot_col(format = "0.0000") has no effect
                     # (that option only applies to Handsontable's numeric
                     # cell type). Pad the decimal columns to 4 places
                     # ourselves before building the table. "p" is excluded
                     # since it already holds text like "< .05" / "> .05".
                     decimal_cols <- c("SS", "MS", "F", "\u03b7\u00b2", "R\u00b2")
                     for (col in decimal_cols) {
                       if (col %in% names(tbl)) {
                         tbl[[col]] <- ifelse(
                           is.na(tbl[[col]]),
                           NA,
                           sprintf("%.4f", as.numeric(tbl[[col]]))
                         )
                       }
                     }
                     
                     ht <- rhandsontable(
                       tbl,
                       rowHeaders = FALSE,
                       width = "100%",
                       useTypes = FALSE
                     ) %>%
                       hot_table(stretchH = "all", highlightRow = TRUE) %>%
                       hot_context_menu(FALSE) %>%
                       hot_cols(readOnly = TRUE)
                     
                   } else if (active_test() == 1) {
                     
                     # Frequency Distribution
                     tbl <- stats$data_table
                     
                     # Relative_Frequency/Cum_Rel_Freq are stored as
                     # plain rounded numerics, so with useTypes = FALSE
                     # below they'd otherwise display with trailing
                     # zeros dropped (e.g. "0.25" instead of "0.2500").
                     # Pre-format them as text to keep 4 decimals, same
                     # as every other numeric answer in the app.
                     tbl$Relative_Frequency <- sprintf("%.4f", tbl$Relative_Frequency)
                     tbl$Cum_Rel_Freq <- sprintf("%.4f", tbl$Cum_Rel_Freq)
                     
                     ht <- rhandsontable(
                       tbl,
                       rowHeaders = FALSE,
                       colHeaders = gsub("_", " ", names(tbl)),
                       width = "100%",
                       useTypes = FALSE
                     ) %>%
                       hot_table(stretchH = "all", highlightRow = TRUE) %>%
                       hot_context_menu(FALSE) %>%
                       hot_cols(readOnly = TRUE)
                     
                   } else if (active_test() == 15 || active_test() == 16) {
                     
                     # Chi-square Goodness of Fit + Homogeneity/Independence
                     tbl <- stats$data_table
                     
                     # E/chi-squared/Cramer's V rows are built by
                     # combining numeric values with a text row (e.g.
                     # "> .05"), which drops trailing zeros - pad those
                     # specific rows back to 4 decimals (uses the
                     # shared helper from utils/pdf_report.R).
                     tbl <- pdf_pad_rows_by_label(tbl, "Statistic", c("E", "\u03c7\u00b2", "Cramer's V"))
                     
                     ht <- rhandsontable(
                       tbl,
                       rowHeaders = FALSE,
                       colHeaders = c("", names(tbl)[-1]),
                       width = "100%",
                       useTypes = FALSE
                     ) %>%
                       hot_table(stretchH = "all", highlightRow = TRUE) %>%
                       hot_context_menu(FALSE) %>%
                       hot_cols(readOnly = TRUE)
                     
                   } else if (active_test() == 4) {
                     
                     # Correlation & Regression
                     tbl <- as.data.frame(t(stats$data_table[1, , drop = FALSE]))
                     tbl$Statistic <- rownames(tbl)
                     rownames(tbl) <- NULL
                     tbl <- tbl[, c("Statistic", names(tbl)[1])]
                     names(tbl)[2] <- "Value"
                     tbl$Value <- pad_decimals(tbl$Value)
                     
                     label_map <- c(
                       X_Mean     = "x\u0304",
                       Y_Mean     = "y\u0304",
                       X_SD       = "SD<sub>X</sub>",
                       Y_SD       = "SD<sub>Y</sub>",
                       SP         = "SP",
                       COV        = "COV",
                       r          = "r",
                       by         = "b<sub>Y</sub>",
                       ay         = "a<sub>Y</sub>",
                       bx         = "b<sub>X</sub>",
                       ax         = "a<sub>X</sub>",
                       SD_XPrime  = "SD<sub>X'</sub>",
                       SD_Yprime  = "SD<sub>Y'</sub>"
                     )
                     tbl$Statistic <- ifelse(
                       tbl$Statistic %in% names(label_map),
                       label_map[tbl$Statistic],
                       tbl$Statistic
                     )
                     
                     ht <- rhandsontable(
                       tbl,
                       rowHeaders = FALSE,
                       width = "100%",
                       useTypes = FALSE
                     ) %>%
                       hot_table(stretchH = "all", highlightRow = TRUE) %>%
                       hot_context_menu(FALSE) %>%
                       hot_cols(readOnly = TRUE) %>%
                       hot_col("Statistic", renderer = "
                         function(instance, td, row, col, prop, value, cellProperties) {
                           td.innerHTML = value;
                           return td;
                         }
                       ") %>%
                       hot_col("Value", format = "0.0000")
                     
                   } else if (active_test() == 2) {
                     
                     # Descriptives
                     tbl <- as.data.frame(t(stats$data_table[1, , drop = FALSE]))
                     tbl$Statistic <- rownames(tbl)
                     rownames(tbl) <- NULL
                     tbl <- tbl[, c("Statistic", names(tbl)[1])]
                     names(tbl)[2] <- "Value"
                     tbl$Value <- pad_decimals(tbl$Value)
                     
                     label_map <- c(
                       Mean  = "Mean (x\u0304)",
                       SkewP = "Skew<sub>p</sub>"
                     )
                     tbl$Statistic <- ifelse(
                       tbl$Statistic %in% names(label_map),
                       label_map[tbl$Statistic],
                       tbl$Statistic
                     )
                     
                     ht <- rhandsontable(
                       tbl,
                       rowHeaders = FALSE,
                       width = "100%",
                       useTypes = FALSE
                     ) %>%
                       hot_table(stretchH = "all", highlightRow = TRUE) %>%
                       hot_context_menu(FALSE) %>%
                       hot_cols(readOnly = TRUE) %>%
                       hot_col("Statistic", renderer = "
                         function(instance, td, row, col, prop, value, cellProperties) {
                           td.innerHTML = value;
                           return td;
                         }
                       ") %>%
                       hot_col("Value", format = "0.0000")
                     
                   } else if (active_test() == 5) {
                     
                     # Single Sample Z-Test
                     tbl <- as.data.frame(t(stats$data_table[1, , drop = FALSE]))
                     tbl$Statistic <- rownames(tbl)
                     rownames(tbl) <- NULL
                     tbl <- tbl[, c("Statistic", names(tbl)[1])]
                     names(tbl)[2] <- "Value"
                     tbl$Value <- pad_decimals(tbl$Value)
                     
                     label_map <- c(
                       H0 = "H<sub>0</sub>",
                       H1 = "H<sub>1</sub>"
                     )
                     tbl$Statistic <- ifelse(
                       tbl$Statistic %in% names(label_map),
                       label_map[tbl$Statistic],
                       tbl$Statistic
                     )
                     
                     ht <- rhandsontable(
                       tbl,
                       rowHeaders = FALSE,
                       width = "100%",
                       useTypes = FALSE
                     ) %>%
                       hot_table(stretchH = "all", highlightRow = TRUE) %>%
                       hot_context_menu(FALSE) %>%
                       hot_cols(readOnly = TRUE) %>%
                       hot_col("Statistic", renderer = "
                         function(instance, td, row, col, prop, value, cellProperties) {
                           td.innerHTML = value;
                           return td;
                         }
                       ")
                     
                   } else if (active_test() == 6) {
                     
                     # Single Sample T-Test
                     tbl <- as.data.frame(t(stats$data_table[1, , drop = FALSE]))
                     tbl$Statistic <- rownames(tbl)
                     rownames(tbl) <- NULL
                     tbl <- tbl[, c("Statistic", names(tbl)[1])]
                     names(tbl)[2] <- "Value"
                     tbl$Value <- pad_decimals(tbl$Value)
                     
                     label_map <- c(
                       H0 = "H<sub>0</sub>",
                       H1 = "H<sub>1</sub>"
                     )
                     tbl$Statistic <- ifelse(
                       tbl$Statistic %in% names(label_map),
                       label_map[tbl$Statistic],
                       tbl$Statistic
                     )
                     
                     ht <- rhandsontable(
                       tbl,
                       rowHeaders = FALSE,
                       width = "100%",
                       useTypes = FALSE
                     ) %>%
                       hot_table(stretchH = "all", highlightRow = TRUE) %>%
                       hot_context_menu(FALSE) %>%
                       hot_cols(readOnly = TRUE) %>%
                       hot_col("Statistic", renderer = "
                         function(instance, td, row, col, prop, value, cellProperties) {
                           td.innerHTML = value;
                           return td;
                         }
                       ")
                     
                   } else if (active_test() == 7) {
                     
                     # Related Samples T-Test
                     tbl <- as.data.frame(t(stats$data_table[1, , drop = FALSE]))
                     tbl$Statistic <- rownames(tbl)
                     rownames(tbl) <- NULL
                     tbl <- tbl[, c("Statistic", names(tbl)[1])]
                     names(tbl)[2] <- "Value"
                     tbl$Value <- pad_decimals(tbl$Value)
                     
                     label_map <- c(
                       H0 = "H<sub>0</sub>",
                       H1 = "H<sub>1</sub>"
                     )
                     tbl$Statistic <- ifelse(
                       tbl$Statistic %in% names(label_map),
                       label_map[tbl$Statistic],
                       tbl$Statistic
                     )
                     
                     ht <- rhandsontable(
                       tbl,
                       rowHeaders = FALSE,
                       width = "100%",
                       useTypes = FALSE
                     ) %>%
                       hot_table(stretchH = "all", highlightRow = TRUE) %>%
                       hot_context_menu(FALSE) %>%
                       hot_cols(readOnly = TRUE) %>%
                       hot_col("Statistic", renderer = "
                         function(instance, td, row, col, prop, value, cellProperties) {
                           td.innerHTML = value;
                           return td;
                         }
                       ")
                     
                   } else if (active_test() == 8) {
                     
                     # Correlation (Advanced)
                     tbl <- as.data.frame(t(stats$data_table[1, , drop = FALSE]))
                     tbl$Statistic <- rownames(tbl)
                     rownames(tbl) <- NULL
                     tbl <- tbl[, c("Statistic", names(tbl)[1])]
                     names(tbl)[2] <- "Value"
                     tbl$Value <- pad_decimals(tbl$Value)
                     
                     label_map <- c(
                       H0 = "H<sub>0</sub>",
                       H1 = "H<sub>1</sub>"
                     )
                     tbl$Statistic <- ifelse(
                       tbl$Statistic %in% names(label_map),
                       label_map[tbl$Statistic],
                       tbl$Statistic
                     )
                     
                     ht <- rhandsontable(
                       tbl,
                       rowHeaders = FALSE,
                       width = "100%",
                       useTypes = FALSE
                     ) %>%
                       hot_table(stretchH = "all", highlightRow = TRUE) %>%
                       hot_context_menu(FALSE) %>%
                       hot_cols(readOnly = TRUE) %>%
                       hot_col("Statistic", renderer = "
                         function(instance, td, row, col, prop, value, cellProperties) {
                           td.innerHTML = value;
                           return td;
                         }
                       ")
                     
                   } else if (active_test() == 9) {
                     
                     # Independent Samples T-Test
                     tbl <- as.data.frame(t(stats$data_table[1, , drop = FALSE]))
                     tbl$Statistic <- rownames(tbl)
                     rownames(tbl) <- NULL
                     tbl <- tbl[, c("Statistic", names(tbl)[1])]
                     names(tbl)[2] <- "Value"
                     tbl$Value <- pad_decimals(tbl$Value)
                     
                     label_map <- c(
                       H0 = "H<sub>0</sub>",
                       H1 = "H<sub>1</sub>"
                     )
                     tbl$Statistic <- ifelse(
                       tbl$Statistic %in% names(label_map),
                       label_map[tbl$Statistic],
                       tbl$Statistic
                     )
                     
                     ht <- rhandsontable(
                       tbl,
                       rowHeaders = FALSE,
                       width = "100%",
                       useTypes = FALSE
                     ) %>%
                       hot_table(stretchH = "all", highlightRow = TRUE) %>%
                       hot_context_menu(FALSE) %>%
                       hot_cols(readOnly = TRUE) %>%
                       hot_col("Statistic", renderer = "
                         function(instance, td, row, col, prop, value, cellProperties) {
                           td.innerHTML = value;
                           return td;
                         }
                       ")
                     
                   } else if (active_test() == 12) {
                     
                     # One-Way ANOVA
                     tbl <- as.data.frame(t(stats$data_table[1, , drop = FALSE]))
                     tbl$Statistic <- rownames(tbl)
                     rownames(tbl) <- NULL
                     tbl <- tbl[, c("Statistic", names(tbl)[1])]
                     names(tbl)[2] <- "Value"
                     tbl$Value <- pad_decimals(tbl$Value)
                     
                     label_map <- c(
                       H0 = "H<sub>0</sub>",
                       H1 = "H<sub>1</sub>"
                     )
                     tbl$Statistic <- ifelse(
                       tbl$Statistic %in% names(label_map),
                       label_map[tbl$Statistic],
                       tbl$Statistic
                     )
                     
                     ht <- rhandsontable(
                       tbl,
                       rowHeaders = FALSE,
                       width = "100%",
                       useTypes = FALSE
                     ) %>%
                       hot_table(stretchH = "all", highlightRow = TRUE) %>%
                       hot_context_menu(FALSE) %>%
                       hot_cols(readOnly = TRUE) %>%
                       hot_col("Statistic", renderer = "
                         function(instance, td, row, col, prop, value, cellProperties) {
                           td.innerHTML = value;
                           return td;
                         }
                       ")
                     
                   } else if (active_test() == 13) {
                     
                     # Multiple Comparisons
                     tbl <- as.data.frame(t(stats$data_table[1, , drop = FALSE]))
                     tbl$Statistic <- rownames(tbl)
                     rownames(tbl) <- NULL
                     tbl <- tbl[, c("Statistic", names(tbl)[1])]
                     names(tbl)[2] <- "Value"
                     tbl$Value <- pad_decimals(tbl$Value)
                     
                     header_labels <- c("Planned Comparisons", "Post-Hoc Comparisons")
                     header_rows_r <- which(tbl$Statistic %in% header_labels)
                     header_rows_js <- header_rows_r - 1
                     
                     bold_rows_js <- paste(header_rows_js, collapse = ",")
                     
                     ht <- rhandsontable(
                       tbl,
                       rowHeaders = FALSE,
                       width = "100%",
                       useTypes = FALSE
                     ) %>%
                       hot_table(
                         stretchH = "all",
                         highlightRow = TRUE,
                       ) %>%
                       hot_context_menu(FALSE) %>%
                       hot_cols(readOnly = TRUE) %>%
                       hot_col("Statistic", renderer = paste0("
                         function(instance, td, row, col, prop, value, cellProperties) {
                           Handsontable.renderers.TextRenderer.apply(this, arguments);
                           var headerRows = [", bold_rows_js, "];
                           if (headerRows.includes(row)) {
                             td.style.fontWeight = 'bold';
                             td.style.textAlign = 'center';
                           }
                           return td;
                         }
                       "))
                     
                   } else {
                     
                     tbl <- as.data.frame(t(stats$data_table[1, , drop = FALSE]))
                     tbl$Statistic <- rownames(tbl)
                     rownames(tbl) <- NULL
                     tbl <- tbl[, c("Statistic", names(tbl)[1])]
                     names(tbl)[2] <- "Value"
                     tbl$Value <- pad_decimals(tbl$Value)
                     
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
                     }
                   }
                   
                   ht
                 })
               })
  
  observeEvent(input$distribution,
               {
                 output$distribution_display = renderPlot(
                   
                   if (active_test() == 3 || active_test() == 17) {
                     
                     ggplot(aes(x = 1:100, y = data), data = plotdata$data) +
                       geom_line() +
                       geom_vline(xintercept = round((
                         stats$p_value * 100
                       )) + .5, color = pal_accent) +
                       theme_void()
                     
                   } else if (active_test() == 5 || active_test() == 6) {
                     
                     ggplot(plotdata$data, aes(x = Group, y = Mean, fill = Group)) +
                       geom_bar(stat = "identity", width = 0.6, color = "black") +
                       geom_errorbar(
                         aes(ymin = Mean - SD, ymax = Mean + SD),
                         width = 0.1,
                         na.rm = TRUE
                       ) +
                       scale_fill_manual(values = pal_two) +
                       scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
                       ylab("Value") +
                       theme_classic() +
                       theme(text = element_text(size = 18), legend.position = "none")
                     
                   } else if (active_test() == 7) {
                     
                     summary_data <- data.frame(
                       Condition = factor(c("Pre", "Post", "Difference"),
                                          levels = c("Pre", "Post", "Difference")),
                       Mean = c(mean(plotdata$data$Pre),
                                mean(plotdata$data$Post),
                                mean(plotdata$data$Diff)),
                       SD   = c(sd(plotdata$data$Pre),
                                sd(plotdata$data$Post),
                                sd(plotdata$data$Diff))
                     )
                     
                     ggplot(summary_data, aes(x = Condition, y = Mean, fill = Condition)) +
                       geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
                       geom_bar(stat = "identity", width = 0.6, color = "black") +
                       geom_errorbar(
                         aes(ymin = Mean - SD, ymax = Mean + SD),
                         width = 0.1,
                         na.rm = TRUE
                       ) +
                       scale_fill_manual(values = pal_multi[1:3]) +
                       scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
                       ylab("Value") +
                       theme_classic() +
                       theme(text = element_text(size = 18), legend.position = "none")
                     
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
                               color = pal_accent
                             ),
                             geom_smooth(method = lm, se = FALSE, color = pal_accent)
                           )
                         else NULL
                       ) +
                       theme_classic() +
                       theme(text = element_text(size = 20))
                     
                   } else if (active_test() == 9) {
                     
                     summary_data <- data.frame(
                       Group = factor(c("Group 1", "Group 2"),
                                      levels = c("Group 1", "Group 2")),
                       Mean  = c(mean(plotdata$data$Data1), mean(plotdata$data$Data2)),
                       SD    = c(sd(plotdata$data$Data1),   sd(plotdata$data$Data2))
                     )
                     
                     ggplot(summary_data, aes(x = Group, y = Mean, fill = Group)) +
                       geom_bar(stat = "identity", width = 0.6, color = "black") +
                       geom_errorbar(
                         aes(ymin = Mean - SD, ymax = Mean + SD),
                         width = 0.1,
                         na.rm = TRUE
                       ) +
                       scale_fill_manual(values = pal_two) +
                       scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
                       ylab("Value") +
                       theme_classic() +
                       theme(text = element_text(size = 18), legend.position = "none")
                     
                   } else if (active_test() == 12) {
                     
                     summary_data <- plotdata$data %>%
                       dplyr::group_by(Group) %>%
                       dplyr::summarise(
                         Mean = mean(Value),
                         SD = sd(Value),
                         .groups = "drop"
                       )
                     
                     ggplot(summary_data, aes(x = Group, y = Mean, fill = Group)) +
                       geom_bar(stat = "identity", width = 0.6, color = "black") +
                       geom_errorbar(
                         aes(ymin = Mean - SD, ymax = Mean + SD),
                         width = 0.1,
                         na.rm = TRUE
                       ) +
                       scale_fill_manual(values = pal_multi) +
                       scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
                       ylab("Value") +
                       theme_classic() +
                       theme(text = element_text(size = 18), legend.position = "none")
                     
                   } else if (active_test() == 13) {
                     
                     summary_data <- plotdata$data %>%
                       dplyr::group_by(Group) %>%
                       dplyr::summarise(
                         Mean = mean(Value),
                         SD = sd(Value),
                         .groups = "drop"
                       )
                     
                     ggplot(summary_data, aes(x = Group, y = Mean, fill = Group)) +
                       geom_bar(stat = "identity", width = 0.6, color = "black") +
                       geom_errorbar(
                         aes(ymin = Mean - SD, ymax = Mean + SD),
                         width = 0.1,
                         na.rm = TRUE
                       ) +
                       scale_fill_manual(values = pal_multi) +
                       scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
                       ylab("Value") +
                       theme_classic() +
                       theme(text = element_text(size = 18), legend.position = "none")
                     
                   } else if (active_test() == 14) {
                     
                     summary_data <- plotdata$data %>%
                       dplyr::group_by(A, B) %>%
                       dplyr::summarise(
                         Mean = mean(Value),
                         SD = sd(Value),
                         .groups = "drop"
                       )
                     
                     ggplot(summary_data, aes(x = A, y = Mean, fill = B)) +
                       
                       geom_bar(
                         stat = "identity",
                         position = position_dodge(width = 0.8),
                         width = 0.7,
                         color = "black"
                       ) +
                       
                       geom_errorbar(
                         aes(ymin = Mean - SD, ymax = Mean + SD),
                         position = position_dodge(width = 0.8),
                         width = 0.15,
                         na.rm = TRUE
                       ) +
                       
                       scale_fill_manual(
                         values = pal_two
                       ) +
                       
                       scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
                       
                       labs(
                         x = "Factor A",
                         y = "Mean",
                         fill = "Factor B"
                       ) +
                       
                       theme_classic() +
                       theme(text = element_text(size = 18))
                     
                   } else if (active_test() == 15) {
                     
                     n_categories <- dplyr::n_distinct(plotdata$data$Category)
                     
                     ggplot(plotdata$data, aes(x = Category, y = Observed, fill = Category)) +
                       geom_bar(stat = "identity", color = "black") +
                       scale_fill_manual(
                         values = colorRampPalette(pal_multi)(n_categories)
                       ) +
                       scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
                       theme_classic() +
                       theme(legend.position = "none")
                     
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
                         values = pal_two
                       ) +
                       
                       scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
                       
                       labs(
                         x = "Category",
                         y = "Frequency",
                         fill = "Group"
                       ) +
                       
                       theme_classic() +
                       theme(text = element_text(size = 18))
                     
                   } else if (active_test() == 10 || active_test() == 11) {
                     
                     # Power (calculate n) + Power (calculate power) -
                     # no meaningful visualization for these problems
                     ggplot() +
                       annotate(
                         "text",
                         x = 0.5,
                         y = 0.5,
                         label = "Nothing to visualize for this problem",
                         size = 6,
                         color = "gray40"
                       ) +
                       xlim(0, 1) +
                       ylim(0, 1) +
                       theme_void()
                     
                   } else {
                     
                     ggplot(aes(x = data), data = plotdata$data) +
                       geom_histogram(color = "black",
                                      fill = pal_two[1],
                                      binwidth = 1) +
                       scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
                       theme_classic()
                   }
                 )
               })
  
  observeEvent(input$refresh, {
    output$stats_display <- renderRHandsontable({ NULL })
    output$distribution_display <- renderPlot({ NULL })
  })
}