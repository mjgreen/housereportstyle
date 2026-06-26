apa_number <- function(value, digits = 2, omit_leading_zero = FALSE, trim_trailing_zero = FALSE) {
  out <- rep("", length(value))
  finite_values <- !is.na(value) & is.finite(value)
  out[!is.na(value) & !is.finite(value)] <- as.character(value[!is.na(value) & !is.finite(value)])

  formatted <- formatC(value[finite_values], format = "f", digits = digits)
  if (trim_trailing_zero) {
    formatted <- sub("\\.?0+$", "", formatted)
  }
  if (omit_leading_zero) {
    formatted <- sub("^(-?)0\\.", "\\1.", formatted)
  }
  out[finite_values] <- formatted
  out
}

apa_table_number <- function(value, digits = 2) {
  out <- rep("", length(value))
  finite_values <- !is.na(value) & is.finite(value)
  whole_values <- finite_values & abs(value - round(value)) < 1e-8
  decimal_values <- finite_values & !whole_values

  out[whole_values] <- as.character(round(value[whole_values]))
  out[decimal_values] <- apa_number(value[decimal_values], digits = digits)
  out[!is.na(value) & !is.finite(value)] <- as.character(value[!is.na(value) & !is.finite(value)])
  out
}

apa_p_table <- function(value) {
  out <- rep("", length(value))
  finite_values <- !is.na(value) & is.finite(value)
  out[finite_values & value < 0.00001] <- "< 0.00001"
  out[finite_values & value >= 0.00001 & value < 0.001] <- formatC(
    value[finite_values & value >= 0.00001 & value < 0.001],
    format = "f",
    digits = 5
  )
  out[finite_values & value >= 0.001] <- formatC(
    value[finite_values & value >= 0.001],
    format = "f",
    digits = 3
  )
  out[!is.na(value) & !is.finite(value)] <- as.character(value[!is.na(value) & !is.finite(value)])
  out
}

apa_p_prose <- function(value) {
  out <- rep("", length(value))
  finite_values <- !is.na(value) & is.finite(value)
  out[finite_values & value < 0.001] <- "p < .001"
  out[finite_values & value >= 0.001] <- paste0(
    "p = ",
    apa_number(value[finite_values & value >= 0.001], digits = 3, omit_leading_zero = TRUE)
  )
  out[!is.na(value) & !is.finite(value)] <- paste0("p = ", value[!is.na(value) & !is.finite(value)])
  out
}

apa_percent <- function(value, digits = 1) {
  paste0(apa_number(100 * value, digits = digits), "%")
}

apa_ci <- function(low, high, digits = 2, omit_leading_zero = FALSE) {
  paste0(
    "[",
    apa_number(low, digits = digits, omit_leading_zero = omit_leading_zero),
    ", ",
    apa_number(high, digits = digits, omit_leading_zero = omit_leading_zero),
    "]"
  )
}

format_apa_table <- function(data, digits = 2, p_cols = NULL) {
  p_cols <- null_coalesce(
    p_cols,
    names(data)[stringr::str_detect(names(data), "(^p$|p_value|p_adjust|pvalue)")]
  )
  p_cols <- intersect(p_cols, names(data))
  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, p_cols)

  formatted <- data
  for (col in p_cols) {
    formatted[[col]] <- apa_p_table(formatted[[col]])
  }
  for (col in numeric_cols) {
    formatted[[col]] <- apa_table_number(formatted[[col]], digits = digits)
  }
  formatted
}

apa_gt <- function(data, label = NULL, title = NULL, note = NULL) {
  table <- data |>
    gt::gt() |>
    gt::tab_options(table.font.size = "small")

  has_label <- !is.null(label) && !is.na(label) && nzchar(label)
  has_title <- !is.null(title) && !is.na(title) && nzchar(title)

  if (has_label || has_title) {
    header_title <- if (has_label) {
      gt::md(paste0("**", label, "**"))
    } else {
      gt::md(paste0("*", title, "*"))
    }
    header_subtitle <- if (has_label && has_title) {
      gt::md(paste0("*", title, "*"))
    } else {
      NULL
    }

    table <- table |>
      gt::tab_header(
        title = header_title,
        subtitle = header_subtitle
      )
  }

  if (!is.null(note) && !is.na(note) && nzchar(note)) {
    table <- table |>
      gt::tab_source_note(gt::md(paste0("*Note.* ", note)))
  }

  table
}

reader_table <- function(data, label = NULL, title = NULL, note = NULL, digits = 2, p_cols = NULL) {
  apa_gt(
    format_apa_table(data, digits = digits, p_cols = p_cols),
    label = label,
    title = title,
    note = note
  )
}

chisq_apa_string <- function(statistic, df, n, p_value, cramers_v, ci_low, ci_high) {
  paste0(
    "chi-square(",
    apa_table_number(df, 0),
    ", N = ",
    apa_table_number(n, 0),
    ") = ",
    apa_number(statistic, 2),
    ", ",
    apa_p_prose(p_value),
    ", Cramer's V = ",
    apa_number(cramers_v, 2),
    ", 95% CI ",
    apa_ci(ci_low, ci_high, 2, omit_leading_zero = TRUE),
    "."
  )
}
