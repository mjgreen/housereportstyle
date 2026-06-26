create_report <- function(path, report_id, title, template = "scientific-html", overwrite = FALSE) {
  if (!nzchar(path) || !nzchar(report_id) || !nzchar(title)) {
    stop("`path`, `report_id`, and `title` must be non-empty strings.")
  }

  template_dir <- system.file("templates", template, package = "housereportstyle")
  template_file <- file.path(template_dir, "report.qmd")
  if (!nzchar(template_file) || !file.exists(template_file)) {
    stop("Could not find template `", template, "` in housereportstyle.")
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "generated"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "generated", "plots"), recursive = TRUE, showWarnings = FALSE)

  destination <- file.path(path, "report.qmd")
  if (file.exists(destination) && !overwrite) {
    stop("Report already exists at `", destination, "`. Use `overwrite = TRUE` to replace it.")
  }

  template_lines <- readLines(template_file, warn = FALSE)
  rendered <- template_lines |>
    stringr::str_replace_all(stringr::fixed("{{REPORT_ID}}"), report_id) |>
    stringr::str_replace_all(stringr::fixed("{{TITLE}}"), title)

  writeLines(rendered, destination)

  css_file <- system.file("templates", "report-style.css", package = "housereportstyle")
  if (nzchar(css_file) && file.exists(css_file)) {
    file.copy(css_file, file.path(path, "report-style.css"), overwrite = overwrite)
  }

  invisible(tibble::tibble(
    path = normalizePath(path, mustWork = FALSE),
    report = normalizePath(destination, mustWork = FALSE),
    css = normalizePath(file.path(path, "report-style.css"), mustWork = FALSE),
    generated = normalizePath(file.path(path, "generated"), mustWork = FALSE),
    plots = normalizePath(file.path(path, "generated", "plots"), mustWork = FALSE),
    template = template
  ))
}
