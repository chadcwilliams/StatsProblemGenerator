####################################################################
####                   PDF Report Generation                    ####
####################################################################
#
# Builds a downloadable PDF containing the problem data, the plot,
# and the answer key for whichever test is currently active.
#
# NOTE ON SCOPE (read before extending):
# The "problem data" table is populated per-test by each script in
# utils/stat_tests/ (currently only one_way_anova.R does this, since
# we are migrating tests to this pattern one at a time). The
# `build_answer_df()` and `build_plot_for_pdf()` helpers below mirror
# the logic already living in observe_events.R for the answer-key
# table and the plot, but return plain data.frame / ggplot objects
# instead of an rhandsontable widget, since gridExtra can't render
# HTML widgets. Only test 12 (One-Way ANOVA) has a fully matching
# plot branch here so far - other tests fall back to a generic
# table (which will work for many of them) and a placeholder plot
# message until they are migrated in the same batch pass as their
# problem-data tables.
####################################################################

# --------------------------------------------------------------
# Shared palette (kept identical to observe_events.R)
# --------------------------------------------------------------
pdf_pal_two    <- c("#F4B8A2", "#A8E0C4")
pdf_pal_multi  <- c("#F4B8A2", "#A8E0C4", "#A8C4F0", "#F0DFA0", "#CBB6EA")
pdf_pal_accent <- "#C97B6A"

# --------------------------------------------------------------
# Small helpers
# --------------------------------------------------------------

# Same rounding/padding behaviour as observe_events.R's pad_decimals,
# duplicated here so this file has no dependency on that closure.
pdf_pad_decimals <- function(x, digits = 4) {
  vapply(x, function(v) {
    v_trim <- trimws(as.character(v))
    if (grepl("^-?[0-9]+\\.[0-9]+$", v_trim)) {
      sprintf(paste0("%.", digits, "f"), as.numeric(v_trim))
    } else {
      v_trim
    }
  }, character(1))
}

# Answer-key labels use HTML (e.g. "SS<sub>G</sub>", "H<sub>0</sub>")
# for on-screen rendering. gridExtra::tableGrob draws plain text by
# default, so as a safe fallback, tags are stripped to a readable
# plain-text equivalent here (e.g. "SS<sub>G</sub>" -> "SS_G"). Real
# subscript/superscript rendering is applied afterward by
# pdf_apply_math_labels() wherever a proper plotmath conversion is
# available - this plain version is what's used if that conversion
# fails for some unanticipated label format.
pdf_strip_html <- function(x) {
  x <- gsub("<sub>(.*?)</sub>", "_\\1", x)
  x <- gsub("<sup>(.*?)</sup>", "^\\1", x)
  x <- gsub("<[^>]+>", "", x)
  x
}

# Maps for the Unicode "Superscripts and Subscripts" block. Some
# renderers/fonts (notably on shinyapps.io's server) don't ship a
# glyph for every character in this block - subscript digits like
# \u2081/\u2082 are common gaps - so labels using them are converted
# to real plotmath subscripts/superscripts (drawn from the ordinary
# digit/letter glyphs, just scaled down) rather than depending on
# those specific Unicode code points being present in the font.
pdf_unicode_sup_map <- c(
  "\u2070" = "0", "\u00b9" = "1", "\u00b2" = "2", "\u00b3" = "3",
  "\u2074" = "4", "\u2075" = "5", "\u2076" = "6", "\u2077" = "7",
  "\u2078" = "8", "\u2079" = "9"
)
pdf_unicode_sub_map <- c(
  "\u2080" = "0", "\u2081" = "1", "\u2082" = "2", "\u2083" = "3",
  "\u2084" = "4", "\u2085" = "5", "\u2086" = "6", "\u2087" = "7",
  "\u2088" = "8", "\u2089" = "9",
  "\u2090" = "a", "\u2091" = "e", "\u2092" = "o", "\u2093" = "x",
  "\u2095" = "h", "\u2096" = "k", "\u2097" = "l", "\u2098" = "m",
  "\u2099" = "n", "\u209a" = "p", "\u209b" = "s", "\u209c" = "t"
)
pdf_unicode_sup_chars <- paste(names(pdf_unicode_sup_map), collapse = "")
pdf_unicode_sub_chars <- paste(names(pdf_unicode_sub_map), collapse = "")

# Converts a run of Unicode superscript/subscript characters to their
# plain digit/letter equivalents, e.g. "\u2081\u2082" -> "12".
pdf_decode_unicode_script <- function(chars, map) {
  if (!nzchar(chars)) return("")
  parts <- strsplit(chars, "")[[1]]
  paste(vapply(parts, function(ch) map[[ch]], character(1)), collapse = "")
}

# Converts a single label into an R plotmath expression, e.g.
# "SS<sub>G</sub>" becomes the expression SS[G], which grid renders
# with a real subscript. Different test scripts encode subscripts
# differently depending on how they're displayed on-screen, so
# several conventions are recognized here:
#   - HTML tags:       "SS<sub>G</sub>", "R<sup>2</sup>"
#     (used by the answer-key widget, which has a custom HTML cell
#     renderer)
#   - a bare bar character with a plain trailing subscript:
#     "x\u0304_G" or just "x\u0304" alone (used by problem-data
#     tables, which render plain text with no HTML renderer)
#   - a bare bar character and/or a base followed by literal Unicode
#     superscript/subscript characters: "n\u2081", "x\u0304\u2082",
#     "s\u00b2\u209a" (used where a test writes the already-styled
#     character directly rather than an HTML tag). These are
#     converted to real plotmath scripts rather than left as literal
#     Unicode, since not every font/renderer ships glyphs for the
#     full Unicode Number Forms block - shinyapps.io's server has
#     been observed rendering these subscript digits as missing-glyph
#     boxes even though they display fine in other environments.
#   - plain "BASE_SUB" text with no bar and no tags: "SD_X"
#     (a fallback for any other test script that just writes an
#     underscore rather than HTML)
#   - plain "BASEdigits" with no separator at all: "H0", "H1"
#     (a fallback for labels that run a trailing digit straight into
#     the base name)
# Returns NULL if the label doesn't match any of these patterns
# (nothing to convert - it's normal text like "n" or "k") or if
# conversion fails for any reason - callers should fall back to the
# plain-text version of the label in that case.
pdf_label_to_expr <- function(label) {
  # A nested <sub> inside another <sub> (e.g. a subscript that itself
  # contains bar(x) with its own subscript) is beyond what this
  # regex-based parser can safely handle - bail out to the plain-text
  # fallback rather than risk a garbled result.
  if (grepl("<sub>.*<sub>|<sup>.*<sup>", label)) return(NULL)

  has_tags <- grepl("<sub>|<sup>", label)

  # A base (optionally bar-topped) followed by a run of Unicode
  # superscript characters, then a run of Unicode subscript
  # characters, with nothing else in the label - e.g. "n\u2081",
  # "x\u0304\u2082", "s\u00b2\u209a", or just "x\u0304" alone.
  # Checked before the plainer bar/underscore cases below so it can
  # handle a bar combined with a Unicode script run, which those
  # simpler patterns don't cover.
  unicode_script_pattern <- paste0(
    "^(x\u0304)?([^", pdf_unicode_sup_chars, pdf_unicode_sub_chars, "]*)",
    "([", pdf_unicode_sup_chars, "]*)([", pdf_unicode_sub_chars, "]*)$"
  )
  has_unicode_script <- !has_tags &&
    grepl(unicode_script_pattern, label) &&
    grepl(paste0("(^x\u0304)|[", pdf_unicode_sup_chars, pdf_unicode_sub_chars, "]"), label)

  has_bar        <- !has_unicode_script && grepl("^x\u0304", label)
  has_plain_sub  <- !has_tags && !has_unicode_script && !has_bar &&
    grepl("^[A-Za-z0-9]+_[A-Za-z0-9']+$", label)
  has_letter_num <- !has_tags && !has_unicode_script && !has_bar && !has_plain_sub &&
    grepl("^[A-Za-z]+[0-9]+$", label)

  if (!has_tags && !has_unicode_script && !has_bar && !has_plain_sub && !has_letter_num) {
    return(NULL)
  }

  tryCatch({
    wrap <- function(x) if (grepl("^[A-Za-z0-9]+$", x)) x else paste0("`", x, "`")

    if (has_tags) {
      rest <- label
      has_bar_tag <- grepl("^x\u0304", rest)
      if (has_bar_tag) rest <- sub("^x\u0304", "", rest)

      base_text <- regmatches(rest, regexpr("^[^<]*", rest))
      rest <- sub("^[^<]*", "", rest)

      base_expr <- if (has_bar_tag) {
        "bar(x)"
      } else if (nzchar(base_text) && grepl("^[A-Za-z0-9]+$", base_text)) {
        base_text
      } else {
        wrap(base_text)
      }

      sub_content <- regmatches(rest, regexpr("(?<=<sub>).*?(?=</sub>)", rest, perl = TRUE))
      sup_content <- regmatches(rest, regexpr("(?<=<sup>).*?(?=</sup>)", rest, perl = TRUE))

      expr_str <- base_expr
      if (length(sub_content) == 1) expr_str <- paste0(expr_str, "[", wrap(sub_content), "]")
      if (length(sup_content) == 1) expr_str <- paste0(expr_str, "^{", sup_content, "}")

    } else if (has_unicode_script) {
      m <- regmatches(label, regexec(unicode_script_pattern, label))[[1]]
      # m[2]=bar marker (or ""), m[3]=plain base text,
      # m[4]=superscript run, m[5]=subscript run
      base_has_bar <- nzchar(m[2])
      base_text <- m[3]
      sup_decoded <- pdf_decode_unicode_script(m[4], pdf_unicode_sup_map)
      sub_decoded <- pdf_decode_unicode_script(m[5], pdf_unicode_sub_map)

      base_expr <- if (base_has_bar && nzchar(base_text)) {
        paste0("bar(x)*", wrap(base_text))
      } else if (base_has_bar) {
        "bar(x)"
      } else if (nzchar(base_text) && grepl("^[A-Za-z0-9]+$", base_text)) {
        base_text
      } else {
        wrap(base_text)
      }

      expr_str <- base_expr
      if (nzchar(sub_decoded)) expr_str <- paste0(expr_str, "[", wrap(sub_decoded), "]")
      if (nzchar(sup_decoded)) expr_str <- paste0(expr_str, "^{", sup_decoded, "}")

    } else if (has_bar && nzchar(sub("^x\u0304", "", label)) &&
               grepl("^_[A-Za-z0-9']+$", sub("^x\u0304", "", label))) {
      # bare bar char plus a trailing plain "_sub" (no HTML tags),
      # e.g. a future test's problem-data label "x\u0304_1"
      expr_str <- paste0("bar(x)[", wrap(sub("^_", "", sub("^x\u0304", "", label))), "]")

    } else if (has_bar) {
      # bare bar char alone (nothing follows) - anything else
      # trailing it that isn't recognized falls through to NULL
      # (plain-text fallback) rather than silently dropping content.
      rest <- sub("^x\u0304", "", label)
      if (nzchar(rest)) return(NULL)
      expr_str <- "bar(x)"

    } else if (has_letter_num) {
      # plain "BASEdigits" convention with no separator, e.g. "H0"
      parts <- regmatches(label, regexec("^([A-Za-z]+)([0-9]+)$", label))[[1]]
      expr_str <- paste0(parts[2], "[", parts[3], "]")

    } else {
      # plain "BASE_SUB" convention (no HTML, no bar), e.g. "SD_X"
      parts <- regmatches(label, regexec("^([A-Za-z0-9]+)_([A-Za-z0-9']+)$", label))[[1]]
      expr_str <- paste0(parts[2], "[", wrap(parts[3]), "]")
    }

    parse(text = expr_str)[[1]]
  }, error = function(e) NULL)
}

# Locates a specific cell grob within a tableGrob's underlying gtable
# (standard lookup pattern from the gridExtra vignette for customizing
# individual cells).
pdf_find_cell <- function(table, row, col, name = "core-fg") {
  l <- table$layout
  which(l$t == row & l$l == col & l$name == name)
}

# Replaces the plain-text grob in one column of a tableGrob with a
# properly rendered plotmath expression, for every row whose original
# (HTML-tagged) label converts successfully. `raw_labels` must be the
# original HTML-tagged labels in the same row order as the table
# (before pdf_strip_html() was applied), so the conversion has the
# <sub>/<sup> tags to work from. Rows that don't need conversion, or
# whose conversion fails, are left as the plain-text fallback that's
# already in the table - this never removes information, only
# upgrades formatting where possible.
pdf_apply_math_labels <- function(tbl_grob, col_index, raw_labels) {
  # Determine the header offset dynamically (rather than assuming a
  # header row is always present) by finding the first data row's "t"
  # position in this column - one fewer moving part to get wrong if
  # a future caller ever passes a headerless table.
  core_rows <- tbl_grob$layout$t[
    tbl_grob$layout$name == "core-fg" & tbl_grob$layout$l == col_index
  ]
  if (length(core_rows) == 0) return(tbl_grob)  # nothing to do
  first_row_t <- min(core_rows)

  for (i in seq_along(raw_labels)) {
    expr <- pdf_label_to_expr(raw_labels[i])
    if (is.null(expr)) next

    idx <- pdf_find_cell(tbl_grob, row = first_row_t + i - 1, col = col_index)
    if (length(idx) != 1) next  # unexpected layout - leave plain text alone

    old_grob <- tbl_grob$grobs[[idx]]
    tbl_grob$grobs[[idx]] <- grid::textGrob(
      label = expr,
      x = old_grob$x, y = old_grob$y,
      just = old_grob$just,
      gp = old_grob$gp
    )
  }
  tbl_grob
}

# Builds a tableGrob from a data.frame and upgrades any convertible
# labels in its label column (subscripts/superscripts/bar-x) to real
# plotmath formatting. Used for both the Problem Data and Answer Key
# pages so any test's label conventions get the same treatment,
# whatever column holds the row labels turns out to be named -
# defaults to "Statistic" (the convention used so far), falling back
# to the first column if that's not present, and skipping the
# upgrade entirely (plain text only) if the table has no columns.
#
# Tables whose column count or cell content scales with the problem
# (e.g. one column per group, or cells holding several stats as
# text) can end up wider or taller than the page - by default,
# gridExtra just draws at natural size and lets the excess run off
# the page edge with no warning. To guard against that generically
# (rather than special-casing specific tests), the table is measured
# at a default font size and, if it doesn't fit within
# `max_width_in` / `max_height_in`, rebuilt at a smaller font size
# scaled down just enough to fit (bounded by `min_base_size` so text
# never shrinks to illegible).
pdf_table_grob <- function(df, cols = names(df), label_col = "Statistic", raw_labels = NULL,
                            max_width_in = 7.3, max_height_in = 9.3,
                            base_size = 11, min_base_size = 6) {
  raw_cols <- cols
  safe_cols <- vapply(cols, pdf_strip_html, character(1), USE.NAMES = FALSE)

  build_raw <- function(size) {
    gridExtra::tableGrob(df, rows = NULL, cols = safe_cols,
                          theme = gridExtra::ttheme_default(base_size = size))
  }

  # Measure natural size on a throwaway device (see pdf_report_page()
  # for why: measuring on the real output device before anything has
  # been drawn silently consumes its first page).
  measure_file <- tempfile(fileext = ".pdf")
  grDevices::cairo_pdf(measure_file, width = 8.5, height = 11)
  tg0 <- build_raw(base_size)
  natural_w <- sum(grid::convertWidth(tg0$widths, "in", valueOnly = TRUE))
  natural_h <- sum(grid::convertHeight(tg0$heights, "in", valueOnly = TRUE))
  grDevices::dev.off()
  unlink(measure_file)

  scale <- min(1, max_width_in / natural_w, max_height_in / natural_h)
  fitted_size <- max(min_base_size, base_size * scale)

  tbl_grob <- if (fitted_size < base_size) build_raw(fitted_size) else tg0

  # Upgrade convertible column headers (e.g. "SD<sub>X</sub>") to real
  # plotmath subscripts/superscripts, the same way row labels are
  # upgraded below - headers live in a separate "colhead-fg" row
  # rather than "core-fg", so they need their own pass.
  tbl_grob <- pdf_apply_math_headers(tbl_grob, raw_headers = raw_cols)

  col_index <- match(label_col, names(df))
  if (is.na(col_index)) col_index <- if (ncol(df) > 0) 1 else NA

  if (!is.na(col_index)) {
    # Prefer explicitly-supplied raw (pre-strip) labels when given -
    # the column itself may already have been through pdf_strip_html
    # by the caller (e.g. build_answer_df's plain-text fallback),
    # which is a lossy transform for some labels (see pdf_report.R's
    # build_answer_df comments), so re-deriving "raw" labels from the
    # already-stripped column isn't always reliable.
    labels_for_math <- if (!is.null(raw_labels)) raw_labels else as.character(df[[col_index]])
    tbl_grob <- pdf_apply_math_labels(
      tbl_grob,
      col_index = col_index,
      raw_labels = labels_for_math
    )
  }

  tbl_grob
}

# Same idea as pdf_apply_math_labels(), but for the header row
# ("colhead-fg" cells, one per column) instead of a single label
# column.
pdf_apply_math_headers <- function(tbl_grob, raw_headers) {
  for (i in seq_along(raw_headers)) {
    expr <- pdf_label_to_expr(raw_headers[i])
    if (is.null(expr)) next

    idx <- pdf_find_cell(tbl_grob, row = 1, col = i, name = "colhead-fg")
    if (length(idx) != 1) next

    old_grob <- tbl_grob$grobs[[idx]]
    tbl_grob$grobs[[idx]] <- grid::textGrob(
      label = expr,
      x = old_grob$x, y = old_grob$y,
      just = old_grob$just,
      gp = old_grob$gp
    )
  }
  tbl_grob
}

# Bolds and centers specific rows in one column of a tableGrob -
# mirrors the on-screen "Planned Comparisons" / "Post-Hoc Comparisons"
# section-header styling used by Multiple Comparisons (test 13).
# `row_indices` are 1-based positions into the data (not counting any
# header row).
pdf_bold_rows <- function(tbl_grob, col_index, row_indices) {
  if (length(row_indices) == 0) return(tbl_grob)

  core_rows <- tbl_grob$layout$t[
    tbl_grob$layout$name == "core-fg" & tbl_grob$layout$l == col_index
  ]
  if (length(core_rows) == 0) return(tbl_grob)
  first_row_t <- min(core_rows)

  for (i in row_indices) {
    idx <- pdf_find_cell(tbl_grob, row = first_row_t + i - 1, col = col_index)
    if (length(idx) != 1) next

    old_grob <- tbl_grob$grobs[[idx]]
    old_gp <- old_grob$gp
    # gpar can't have both $font and $fontface set at once; strip
    # $font (if the original cell had one) before adding $fontface.
    if (!is.null(old_gp) && !is.null(old_gp$font)) old_gp$font <- NULL
    new_gp <- if (is.null(old_gp)) grid::gpar(fontface = "bold") else {
      old_gp$fontface <- "bold"
      old_gp
    }
    tbl_grob$grobs[[idx]] <- grid::textGrob(
      label = old_grob$label,
      x = 0.5, y = old_grob$y,
      just = "center",
      gp = new_gp
    )
  }
  tbl_grob
}

# Pads only specific named columns to a fixed number of decimals,
# leaving everything else (including text columns like "p" that hold
# "< .05" / "> .05") untouched. Mirrors the per-test_id decimal_cols
# selection observe_events.R applies for test 14.
pdf_pad_specific_cols <- function(df, cols, digits = 4) {
  for (col in cols) {
    if (col %in% names(df)) {
      df[[col]] <- ifelse(
        is.na(df[[col]]),
        NA,
        sprintf(paste0("%.", digits, "f"), suppressWarnings(as.numeric(df[[col]])))
      )
    }
  }
  df
}

# Pads numeric-looking cells to a fixed number of decimals for
# specific ROWS (identified by their value in `label_col`), rather
# than specific columns - needed for tables like the Chi-Squared
# answer keys, where a row is built via rbind()-ing numeric vectors
# together with a text row (e.g. "> .05"), which coerces the whole
# column to character and silently drops trailing zeros
# (sprintf/round output like 50 instead of "50.0000"). Cells that are
# NA, blank, or don't parse as numeric are left untouched.
pdf_pad_rows_by_label <- function(df, label_col, target_labels, digits = 4) {
  if (!(label_col %in% names(df))) return(df)
  row_idx <- which(df[[label_col]] %in% target_labels)
  if (length(row_idx) == 0) return(df)

  value_cols <- setdiff(names(df), label_col)
  for (col in value_cols) {
    vals <- df[[col]][row_idx]
    numeric_vals <- suppressWarnings(as.numeric(vals))
    can_pad <- !is.na(vals) & vals != "" & !is.na(numeric_vals)
    vals[can_pad] <- sprintf(paste0("%.", digits, "f"), numeric_vals[can_pad])
    df[[col]][row_idx] <- vals
  }
  df
}

# Applies a named label_map (old name -> display label) to a column,
# mirroring the `tbl$Statistic <- ifelse(tbl$Statistic %in% ...)`
# pattern used throughout observe_events.R.
pdf_apply_label_map <- function(x, label_map) {
  ifelse(x %in% names(label_map), label_map[x], x)
}

# Transposes a single-row stats$data_table into a two-column
# Statistic/Value data.frame - the shape used by most (but not all)
# answer keys. Mirrors the repeated
#   tbl <- as.data.frame(t(stats$data_table[1, , drop = FALSE]))
#   ...
# block that appears throughout observe_events.R.
pdf_transpose_single_row <- function(stats) {
  tbl <- as.data.frame(t(stats$data_table[1, , drop = FALSE]))
  tbl$Statistic <- rownames(tbl)
  rownames(tbl) <- NULL
  tbl <- tbl[, c("Statistic", names(tbl)[1])]
  names(tbl)[2] <- "Value"
  tbl
}

# --------------------------------------------------------------
# Answer key table -> plain data.frame
# --------------------------------------------------------------
# Mirrors the branch structure in observe_events.R's `input$answers`
# handler test-id by test-id. Returns a data.frame whose Statistic (or
# equivalent) column has already been through pdf_strip_html() as a
# safe plain-text fallback; pdf_table_grob() upgrades whichever labels
# convert cleanly to real plotmath subscripts/superscripts.
#
# Metadata the caller needs (custom column headers, which column to
# treat as the label column, and any rows that should be bolded) is
# attached as attributes rather than a second return value, so this
# stays a plain data.frame everywhere else it's used.
build_answer_df <- function(test_id, stats) {

  tbl <- NULL
  col_headers <- NULL
  label_col <- "Statistic"
  bold_rows <- integer(0)

  if (test_id == 14) {
    # Multifactorial ANOVA - full table, not transposed.
    tbl <- stats$data_table
    tbl <- pdf_pad_specific_cols(tbl, c("SS", "MS", "F", "\u03b7\u00b2", "R\u00b2"))
    label_col <- "Source"

  } else if (test_id == 1) {
    # Frequency Distribution - full table, not transposed.
    tbl <- stats$data_table
    # Relative_Frequency/Cum_Rel_Freq are stored as plain rounded
    # numerics, so converting to display text drops trailing zeros
    # (e.g. 0.5 instead of 0.5000) - pad them to 4 decimals like every
    # other test's answer key. Frequency/Cumulative_Frequency are
    # counts and stay as plain integers.
    tbl <- pdf_pad_specific_cols(tbl, c("Relative_Frequency", "Cum_Rel_Freq"))
    col_headers <- gsub("_", " ", names(tbl))
    label_col <- "Data"

  } else if (test_id %in% c(15, 16)) {
    # Chi-Squared (Goodness of Fit / Homogeneity & Independence) -
    # full table, not transposed. The E/expected row (test 15) and
    # the chi-squared/Cramer's V values (both tests) are built by
    # combining numeric values with a text row (e.g. "> .05") via
    # rbind()/as.character(), which drops trailing zeros - pad those
    # specific rows back to 4 decimals. (Whichever labels don't apply
    # to a given test are simply absent, so this is safe for both.)
    tbl <- stats$data_table
    tbl <- pdf_pad_rows_by_label(tbl, "Statistic", c("E", "\u03c7\u00b2", "Cramer's V"))
    col_headers <- c("", names(tbl)[-1])
    label_col <- "Statistic"

  } else if (test_id == 4) {
    # Correlation & Regression
    tbl <- pdf_transpose_single_row(stats)
    label_map <- c(
      X_Mean = "x\u0304", Y_Mean = "y\u0304",
      X_SD = "SD<sub>X</sub>", Y_SD = "SD<sub>Y</sub>",
      SP = "SP", COV = "COV", r = "r",
      by = "b<sub>Y</sub>", ay = "a<sub>Y</sub>",
      bx = "b<sub>X</sub>", ax = "a<sub>X</sub>",
      SD_XPrime = "SD<sub>X'</sub>", SD_Yprime = "SD<sub>Y'</sub>"
    )
    tbl$Statistic <- pdf_apply_label_map(tbl$Statistic, label_map)

  } else if (test_id == 2) {
    # Descriptives
    tbl <- pdf_transpose_single_row(stats)
    label_map <- c(Mean = "Mean (x\u0304)", SkewP = "Skew<sub>p</sub>")
    tbl$Statistic <- pdf_apply_label_map(tbl$Statistic, label_map)

  } else if (test_id %in% c(5, 6, 7, 8, 12)) {
    # Single Sample Z-Test, Single Sample T-Test, Related Samples
    # T-Test, Correlation (Advanced), One-Way ANOVA - all share the
    # same H0/H1 label map.
    tbl <- pdf_transpose_single_row(stats)
    label_map <- c(H0 = "H<sub>0</sub>", H1 = "H<sub>1</sub>")
    tbl$Statistic <- pdf_apply_label_map(tbl$Statistic, label_map)

  } else if (test_id == 9) {
    # Independent Samples T-Test - H0/H1 map, plus a nested-subscript
    # label ("standard error of x\u0304\u2081 - x\u0304\u2082") that's beyond what the
    # generic HTML-tag parser can convert (a <sub> nested inside
    # another <sub>), so it's swapped for an equivalent flattened form
    # first: the whole "x\u03041-x\u03042" becomes a single subscript block
    # (not a sub-within-a-sub), using plain ASCII digits rather than
    # Unicode subscript-digit characters - it's already rendered at
    # reduced size as one flat subscript, so there's no need for those
    # special glyphs, and plain digits are guaranteed to be in any font.
    tbl <- pdf_transpose_single_row(stats)
    label_map <- c(
      H0 = "H<sub>0</sub>", H1 = "H<sub>1</sub>",
      "s<sub>x\u0304<sub>1</sub>-x\u0304<sub>2</sub></sub>" = "s<sub>x\u03041-x\u03042</sub>"
    )
    tbl$Statistic <- pdf_apply_label_map(tbl$Statistic, label_map)

  } else if (test_id == 13) {
    # Multiple Comparisons - bold/centered section-header rows.
    tbl <- pdf_transpose_single_row(stats)
    header_labels <- c("Planned Comparisons", "Post-Hoc Comparisons")
    bold_rows <- which(tbl$Statistic %in% header_labels)

  } else {
    # 3, 10, 11, 17 and any unmigrated test: generic transpose.
    tbl <- pdf_transpose_single_row(stats)
  }

  raw_labels <- if (label_col %in% names(tbl)) as.character(tbl[[label_col]]) else NULL

  if (label_col %in% names(tbl)) {
    tbl[[label_col]] <- pdf_strip_html(tbl[[label_col]])
  }
  if ("Value" %in% names(tbl)) {
    tbl$Value <- pdf_pad_decimals(tbl$Value)
  }

  attr(tbl, "col_headers") <- col_headers
  attr(tbl, "label_col") <- label_col
  attr(tbl, "bold_rows") <- bold_rows
  attr(tbl, "raw_label_values") <- raw_labels
  tbl
}

# --------------------------------------------------------------
# Plot -> ggplot object
# --------------------------------------------------------------
# Mirrors the branch structure in observe_events.R's
# `input$distribution` handler test-id by test-id.
build_plot_for_pdf <- function(test_id, plotdata, stats) {

  if (test_id == 3 || test_id == 17) {

    ggplot(aes(x = 1:100, y = data), data = plotdata$data) +
      geom_line() +
      geom_vline(
        xintercept = round((stats$p_value * 100)) + .5,
        color = pdf_pal_accent
      ) +
      theme_void()

  } else if (test_id == 5 || test_id == 6) {

    ggplot(plotdata$data, aes(x = Group, y = Mean, fill = Group)) +
      geom_bar(stat = "identity", width = 0.6, color = "black") +
      geom_errorbar(
        aes(ymin = Mean - SD, ymax = Mean + SD),
        width = 0.1,
        na.rm = TRUE
      ) +
      scale_fill_manual(values = pdf_pal_two) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      ylab("Value") +
      theme_classic() +
      theme(text = element_text(size = 18), legend.position = "none")

  } else if (test_id == 7) {

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
      scale_fill_manual(values = pdf_pal_multi[1:3]) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      ylab("Value") +
      theme_classic() +
      theme(text = element_text(size = 18), legend.position = "none")

  } else if (test_id == 4 || test_id == 8) {

    p <- ggplot(aes(x = X, y = Y), data = plotdata$data) +
      geom_point(size = 4, alpha = .5)

    if (test_id == 4) {
      p <- p +
        geom_segment(
          y = min(plotdata$data$Y),
          x = stats$data_table$ax + stats$data_table$bx * min(plotdata$data$Y),
          yend = max(plotdata$data$Y),
          xend = stats$data_table$ax + stats$data_table$bx * max(plotdata$data$Y),
          color = pdf_pal_accent
        ) +
        geom_smooth(method = lm, se = FALSE, color = pdf_pal_accent)
    }

    p + theme_classic() + theme(text = element_text(size = 20))

  } else if (test_id == 9) {

    summary_data <- data.frame(
      Group = factor(c("Group 1", "Group 2"), levels = c("Group 1", "Group 2")),
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
      scale_fill_manual(values = pdf_pal_two) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      ylab("Value") +
      theme_classic() +
      theme(text = element_text(size = 18), legend.position = "none")

  } else if (test_id == 12 || test_id == 13) {

    summary_data <- plotdata$data %>%
      dplyr::group_by(Group) %>%
      dplyr::summarise(Mean = mean(Value), SD = sd(Value), .groups = "drop")

    ggplot(summary_data, aes(x = Group, y = Mean, fill = Group)) +
      geom_bar(stat = "identity", width = 0.6, color = "black") +
      geom_errorbar(
        aes(ymin = Mean - SD, ymax = Mean + SD),
        width = 0.1,
        na.rm = TRUE
      ) +
      scale_fill_manual(values = pdf_pal_multi) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      ylab("Value") +
      theme_classic() +
      theme(text = element_text(size = 18), legend.position = "none")

  } else if (test_id == 14) {

    summary_data <- plotdata$data %>%
      dplyr::group_by(A, B) %>%
      dplyr::summarise(Mean = mean(Value), SD = sd(Value), .groups = "drop")

    ggplot(summary_data, aes(x = A, y = Mean, fill = B)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black") +
      geom_errorbar(
        aes(ymin = Mean - SD, ymax = Mean + SD),
        position = position_dodge(width = 0.8),
        width = 0.15,
        na.rm = TRUE
      ) +
      scale_fill_manual(values = pdf_pal_two) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Factor A", y = "Mean", fill = "Factor B") +
      theme_classic() +
      theme(text = element_text(size = 18))

  } else if (test_id == 15) {

    n_categories <- dplyr::n_distinct(plotdata$data$Category)

    ggplot(plotdata$data, aes(x = Category, y = Observed, fill = Category)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_manual(values = colorRampPalette(pdf_pal_multi)(n_categories)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_classic() +
      theme(legend.position = "none")

  } else if (test_id == 16) {

    ggplot(plotdata$data, aes(x = Category, y = Count, fill = Group)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black") +
      scale_fill_manual(values = pdf_pal_two) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Category", y = "Frequency", fill = "Group") +
      theme_classic() +
      theme(text = element_text(size = 18))

  } else if (test_id == 10 || test_id == 11) {

    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "Nothing to visualize for this problem",
               size = 6, color = "gray40") +
      xlim(0, 1) + ylim(0, 1) +
      theme_void()

  } else {
    # 1, 2, and any unmigrated test: histogram of raw scores.
    ggplot(aes(x = data), data = plotdata$data) +
      geom_histogram(color = "black", fill = pdf_pal_two[1], binwidth = 1) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_classic()
  }
}

# --------------------------------------------------------------
# One-page layout: title anchored near the top, then content.
# Tables are drawn at their natural size right below the title
# (rather than being stretched/centered across the whole page);
# plots (fill_page = TRUE) expand to fill the remaining space.
# --------------------------------------------------------------
pdf_report_page <- function(title, content_grob, fill_page = FALSE, footnote = NULL) {

  # content_grob is often passed as an unevaluated call (e.g.
  # pdf_table_grob(...)), and R only evaluates function arguments
  # lazily on first use. pdf_table_grob() itself opens and closes a
  # throwaway measurement device internally - if its evaluation were
  # left to happen lazily inside the measurement block below, that
  # would nest one device-open/close cycle inside another, which
  # leaves the graphics device stack in the wrong state and produces
  # a blank page. Forcing evaluation here, before any device work
  # starts, keeps every device open/close pair properly nested.
  force(content_grob)

  title_grob <- grid::textGrob(
    title,
    gp = grid::gpar(fontsize = 16, fontface = "bold")
  )

  # Small gray footnote (currently used for "Seed: ..."), left-aligned
  # near the bottom of the page. A blank placeholder grob is used when
  # there's no footnote, so the row layout stays identical either way.
  footnote_grob <- if (!is.null(footnote)) {
    grid::textGrob(
      footnote,
      x = 0, hjust = 0,
      gp = grid::gpar(fontsize = 9, col = "gray40")
    )
  } else {
    grid::nullGrob()
  }
  footnote_height <- 0.25

  if (fill_page) {
    gridExtra::grid.arrange(
      title_grob,
      content_grob,
      footnote_grob,
      heights = grid::unit(c(0.6, 1, footnote_height), c("in", "null", "in")),
      ncol = 1
    )
  } else {
    # Measuring a grob's height (grobHeight/convertHeight) needs an
    # active graphics device for font metrics. Doing that measurement
    # on the real output device - before any content has been drawn -
    # silently consumes the first page and leaves it blank. A
    # throwaway device is opened just for the measurement and closed
    # immediately, so the real device's first page is only ever used
    # by an actual grid.arrange() draw call. cairo_pdf (not the base
    # pdf() device) is used for the measurement too, since the answer
    # keys contain unicode glyphs (x\u0304, \u03b7\u00b2) that the base
    # device's font metrics can't measure correctly.
    measure_file <- tempfile(fileext = ".pdf")
    grDevices::cairo_pdf(measure_file, width = 8.5, height = 11)
    # grid::grobHeight() on a tableGrob under-reports its true height
    # (it does not sum every row correctly), so the row heights of the
    # underlying gtable are summed directly instead.
    if (!is.null(content_grob$heights)) {
      content_height <- sum(grid::convertHeight(content_grob$heights, "in", valueOnly = TRUE))
    } else {
      content_height <- grid::convertHeight(
        grid::grobHeight(content_grob), "in", valueOnly = TRUE
      )
    }
    grDevices::dev.off()
    unlink(measure_file)

    gridExtra::grid.arrange(
      title_grob,
      content_grob,
      grid::nullGrob(),
      footnote_grob,
      heights = grid::unit(
        c(0.6, content_height + 0.3, 1, footnote_height),
        c("in", "in", "null", "in")
      ),
      ncol = 1
    )
  }
}

# --------------------------------------------------------------
# Main entry point
# --------------------------------------------------------------
# problemdata : reactiveValues with $table (data.frame) and
#               $col_headers (character vector, or NULL)
# stats       : reactiveValues with $data_table (answer key source)
# plotdata    : reactiveValues with $data (plot source)
# test_id     : numeric/character test id (e.g. 12)
# test_name   : display name for page titles (e.g. "One-Way ANOVA")
#
# Returns the path to the generated PDF (a tempfile).
generate_pdf_report <- function(problemdata, stats, plotdata, test_id, test_name) {

  if (is.null(problemdata$table)) {
    stop("No problem data available yet - generate a data set before downloading.")
  }

  out_path <- tempfile(fileext = ".pdf")

  # cairo_pdf supports the unicode glyphs used throughout the answer
  # keys (e.g. x\u0304, \u03b7\u00b2), which the default pdf() device
  # may not render correctly depending on the system's fonts.
  grDevices::cairo_pdf(out_path, width = 8.5, height = 11, onefile = TRUE)

  # ---- Page 1: Problem data ----
  problem_tbl <- problemdata$table
  col_labels <- problemdata$col_headers
  if (is.null(col_labels)) col_labels <- names(problem_tbl)
  label_col <- problemdata$label_col
  if (is.null(label_col)) label_col <- "Statistic"

  seed_footnote <- if (!is.null(problemdata$seed)) {
    paste0("Seed: ", problemdata$seed)
  } else {
    NULL
  }

  pdf_report_page(
    title = paste0(test_name, " \u2014 Problem Data"),
    content_grob = pdf_table_grob(problem_tbl, cols = col_labels, label_col = label_col),
    footnote = seed_footnote
  )

  # ---- Page 2: Plot ----
  # (pdf_report_page() advances to a new page on its own via
  # grid.arrange(), so no manual grid.newpage() call is needed here -
  # adding one would insert a blank page.)
  p <- build_plot_for_pdf(test_id, plotdata, stats)
  pdf_report_page(
    title = paste0(test_name, " \u2014 Plot"),
    content_grob = p,
    fill_page = TRUE,
    footnote = seed_footnote
  )

  # ---- Page 3: Answer key ----
  ans_tbl <- build_answer_df(test_id, stats)
  ans_col_headers <- attr(ans_tbl, "col_headers")
  ans_label_col <- attr(ans_tbl, "label_col")
  ans_bold_rows <- attr(ans_tbl, "bold_rows")
  ans_raw_labels <- attr(ans_tbl, "raw_label_values")
  if (is.null(ans_label_col)) ans_label_col <- "Statistic"

  ans_grob <- pdf_table_grob(
    ans_tbl,
    cols = if (is.null(ans_col_headers)) names(ans_tbl) else ans_col_headers,
    label_col = ans_label_col,
    raw_labels = ans_raw_labels
  )
  if (length(ans_bold_rows) > 0) {
    ans_col_index <- match(ans_label_col, names(ans_tbl))
    if (!is.na(ans_col_index)) {
      ans_grob <- pdf_bold_rows(ans_grob, col_index = ans_col_index, row_indices = ans_bold_rows)
    }
  }

  pdf_report_page(
    title = paste0(test_name, " \u2014 Answer Key"),
    content_grob = ans_grob,
    footnote = seed_footnote
  )

  grDevices::dev.off()

  out_path
}
