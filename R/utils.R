null_coalesce <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

finite_or_na <- function(x) {
  x[is.finite(x) & !is.na(x)]
}

median_or_na <- function(x) {
  x <- finite_or_na(x)
  if (length(x) == 0) {
    return(NA_real_)
  }
  stats::median(x)
}

min_or_na <- function(x) {
  x <- finite_or_na(x)
  if (length(x) == 0) {
    return(NA_real_)
  }
  min(x)
}

cluster_label <- function(value) {
  paste0("Cluster ", as.character(value))
}

safe_analysis_name <- function(value) {
  value |>
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}

