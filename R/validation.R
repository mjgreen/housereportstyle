parse_qmd_chunks <- function(lines) {
  starts <- which(grepl("^```\\{r", lines))
  if (length(starts) == 0) {
    return(tibble::tibble(label = character(), start = integer(), end = integer(), options = list()))
  }

  chunks <- lapply(starts, function(start) {
    end_candidates <- which(seq_along(lines) > start & grepl("^```\\s*$", lines))
    end <- if (length(end_candidates) == 0) length(lines) else end_candidates[[1]]
    header <- lines[[start]]
    label <- sub("^```\\{r\\s*", "", header)
    label <- sub("[,}].*$", "", label)
    label <- trimws(label)
    option_lines <- lines[seq.int(start + 1, max(start + 1, end - 1))]
    option_lines <- option_lines[grepl("^#\\|", option_lines)]
    tibble::tibble(label = label, start = start, end = end, options = list(option_lines))
  })

  dplyr::bind_rows(chunks)
}

chunk_has_option <- function(options, pattern) {
  any(grepl(pattern, options, perl = TRUE))
}

line_in_code_chunk <- function(index, chunks) {
  if (nrow(chunks) == 0) {
    return(FALSE)
  }
  any(index >= chunks$start & index <= chunks$end)
}

has_aside_after_chunk <- function(lines, chunk_end, lookahead = 12) {
  if (chunk_end >= length(lines)) {
    return(FALSE)
  }
  search_range <- seq.int(chunk_end + 1, min(length(lines), chunk_end + lookahead))
  any(grepl("^::: \\{\\.aside\\}", lines[search_range]))
}

detect_baked_plot_labels <- function(lines) {
  in_labs <- FALSE
  for (line in lines) {
    if (!in_labs && grepl("labs\\s*\\(", line, perl = TRUE)) {
      after_labs <- sub("^.*labs\\s*\\(", "", line, perl = TRUE)
      if (grepl("\\b(title|subtitle|caption)\\s*=", after_labs, perl = TRUE)) {
        return(TRUE)
      }
      in_labs <- !grepl("\\)", after_labs, perl = TRUE)
      next
    }

    if (in_labs) {
      if (grepl("^\\s*(title|subtitle|caption)\\s*=", line, perl = TRUE)) {
        return(TRUE)
      }
      if (grepl("\\)", line, perl = TRUE)) {
        in_labs <- FALSE
      }
    }
  }

  FALSE
}

yaml_has_line <- function(lines, pattern) {
  any(grepl(pattern, lines, perl = TRUE))
}

validate_report_style <- function(qmd_path, html_path = NULL) {
  if (!file.exists(qmd_path)) {
    stop("Could not find qmd file: ", qmd_path)
  }

  lines <- readLines(qmd_path, warn = FALSE)
  chunks <- parse_qmd_chunks(lines)

  add_check <- function(check, severity, passed, message) {
    tibble::tibble(
      check = check,
      severity = severity,
      passed = isTRUE(passed),
      message = message
    )
  }

  figure_chunks <- chunks |>
    dplyr::filter(grepl("^fig-", .data$label))
  model_chunks <- chunks |>
    dplyr::filter(grepl("(_model$|_model_)", .data$label))
  display_chunks <- chunks |>
    dplyr::filter(grepl("(^fig-|table|tables)", .data$label))

  hardcoded_figure_lines <- which(grepl("\\bFigure\\s+[0-9]+\\b", lines))
  hardcoded_figure_lines <- hardcoded_figure_lines[
    !vapply(hardcoded_figure_lines, line_in_code_chunk, logical(1), chunks = chunks)
  ]
  inline_code_heading_lines <- which(grepl("^#{1,6}\\s+.*`", lines))
  inline_code_heading_lines <- inline_code_heading_lines[
    !vapply(inline_code_heading_lines, line_in_code_chunk, logical(1), chunks = chunks)
  ]
  appendix_heading_lines <- which(grepl("^#{2,6}\\s+Appendix\\s+", lines))
  appendix_headings_ok <- length(appendix_heading_lines) == 0 ||
    all(grepl("\\{\\.unnumbered\\}", lines[appendix_heading_lines], fixed = FALSE))

  stale_ok <- TRUE
  stale_message <- "No HTML path supplied; stale-render check skipped."
  if (!is.null(html_path)) {
    if (!file.exists(html_path)) {
      stale_ok <- FALSE
      stale_message <- paste0("HTML file does not exist: ", html_path)
    } else {
      stale_ok <- file.info(html_path)$mtime >= file.info(qmd_path)$mtime
      stale_message <- if (stale_ok) {
        "HTML file is at least as recent as the QMD source."
      } else {
        "HTML file is older than the QMD source; re-render before sharing."
      }
    }
  }

  figure_caption_ok <- nrow(figure_chunks) == 0 || all(vapply(
    figure_chunks$options,
    chunk_has_option,
    logical(1),
    pattern = "fig-cap-location:\\s*top"
  ))

  figure_asides_ok <- nrow(figure_chunks) == 0 || all(vapply(
    figure_chunks$end,
    has_aside_after_chunk,
    logical(1),
    lines = lines
  ))

  model_cache_ok <- nrow(model_chunks) == 0 || all(vapply(
    model_chunks$options,
    chunk_has_option,
    logical(1),
    pattern = "cache:\\s*true"
  ))

  display_dependson_ok <- nrow(display_chunks) == 0 || all(vapply(
    display_chunks$options,
    chunk_has_option,
    logical(1),
    pattern = "dependson:"
  ))

  dplyr::bind_rows(
    add_check(
      "html house style enabled",
      "warning",
      all(c(
        yaml_has_line(lines, "^\\s*toc:\\s*true\\s*$"),
        yaml_has_line(lines, "^\\s*toc-title:\\s*[\"']?Contents[\"']?\\s*$"),
        yaml_has_line(lines, "^\\s*toc-depth:\\s*3\\s*$"),
        yaml_has_line(lines, "^\\s*toc-location:\\s*left\\s*$"),
        yaml_has_line(lines, "^\\s*number-sections:\\s*true\\s*$"),
        yaml_has_line(lines, "^\\s*number-depth:\\s*3\\s*$"),
        yaml_has_line(lines, "^\\s*light:\\s*cosmo\\s*$"),
        yaml_has_line(lines, "^\\s*dark:\\s*darkly\\s*$"),
        yaml_has_line(lines, "^\\s*respect-user-color-scheme:\\s*true\\s*$"),
        yaml_has_line(lines, "^\\s*embed-resources:\\s*true\\s*$"),
        yaml_has_line(lines, "^\\s*smooth-scroll:\\s*true\\s*$"),
        yaml_has_line(lines, "^\\s*anchor-sections:\\s*true\\s*$"),
        yaml_has_line(lines, "^\\s*df-print:\\s*paged\\s*$"),
        yaml_has_line(lines, "^\\s*link-external-icon:\\s*true\\s*$"),
        yaml_has_line(lines, "^\\s*css:\\s*report-style[.]css\\s*$")
      )),
      "Use the shared Quarto HTML house style: floating left TOC, numbered sections, light/dark themes, embedded resources, smooth anchors, external-link icon, paged data frames, and report-style.css."
    ),
    add_check(
      "no inline code in headings",
      "warning",
      length(inline_code_heading_lines) == 0,
      "Use plain text in headings; inline code pills are visually heavy, especially in dark mode."
    ),
    add_check(
      "appendix headings are lettered and unnumbered",
      "warning",
      appendix_headings_ok,
      "Use lettered appendix headings such as `## Appendix A. Metrics {.unnumbered}`."
    ),
    add_check(
      "no kable output",
      "warning",
      !any(grepl("\\bkable\\s*\\(", lines)),
      "Use gt-backed reader_table() for reader-facing tables."
    ),
    add_check(
      "no baked ggplot figure titles or notes",
      "warning",
      !detect_baked_plot_labels(lines),
      "Quarto should own figure titles/numbers through fig-cap; notes belong in margin asides."
    ),
    add_check(
      "figure captions placed above figures",
      "warning",
      figure_caption_ok,
      "Each fig-* chunk should include `#| fig-cap-location: top`."
    ),
    add_check(
      "figure notes in adjacent asides",
      "warning",
      figure_asides_ok,
      "Each fig-* chunk should be followed by a margin aside containing the figure note."
    ),
    add_check(
      "no hard-coded figure numbers in prose",
      "warning",
      length(hardcoded_figure_lines) == 0,
      "Use Quarto cross-references such as `@fig-example` instead of prose like `Figure 1`."
    ),
    add_check(
      "model chunks cached",
      "warning",
      model_cache_ok,
      "Expensive model chunks should use `#| cache: true`."
    ),
    add_check(
      "display chunks declare dependencies",
      "warning",
      display_dependson_ok,
      "Display chunks should use `#| dependson:` so style edits can re-render quickly."
    ),
    add_check(
      "rendered html is not stale",
      "warning",
      stale_ok,
      stale_message
    )
  )
}
