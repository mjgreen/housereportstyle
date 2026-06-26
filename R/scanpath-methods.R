normalize_scanpath_geometry <- function(geometry) {
  required <- c("row_min", "row_max", "col_min", "col_max")
  missing <- setdiff(required, names(geometry))
  if (length(missing) > 0) {
    stop("Scanpath geometry is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  geometry <- as.list(geometry)
  geometry$row_min <- as.integer(geometry$row_min)
  geometry$row_max <- as.integer(geometry$row_max)
  geometry$col_min <- as.integer(geometry$col_min)
  geometry$col_max <- as.integer(geometry$col_max)

  if (
    any(!is.finite(unlist(geometry[required], use.names = FALSE))) ||
      geometry$row_max < geometry$row_min ||
      geometry$col_max < geometry$col_min
  ) {
    stop("Scanpath geometry must have finite ordered row/column bounds.", call. = FALSE)
  }

  geometry$n_rows <- as.integer(geometry$row_max - geometry$row_min + 1L)
  geometry$n_cols <- as.integer(geometry$col_max - geometry$col_min + 1L)
  if (is.null(geometry$source) || length(geometry$source) == 0 || is.na(geometry$source)) {
    geometry$source <- "manual"
  }
  geometry
}

detect_scanpath_geometry <- function(events, geometry = NULL, row_col = "row", col_col = "col") {
  if (!is.null(geometry) && !identical(geometry, "auto")) {
    return(normalize_scanpath_geometry(geometry))
  }

  embedded_cols <- c(
    "geometry_row_min", "geometry_row_max", "geometry_col_min", "geometry_col_max"
  )
  if (all(embedded_cols %in% names(events))) {
    embedded <- events |>
      dplyr::summarise(
        row_min = dplyr::first(stats::na.omit(.data$geometry_row_min)),
        row_max = dplyr::first(stats::na.omit(.data$geometry_row_max)),
        col_min = dplyr::first(stats::na.omit(.data$geometry_col_min)),
        col_max = dplyr::first(stats::na.omit(.data$geometry_col_max))
      ) |>
      as.list()

    if (all(vapply(embedded, function(value) length(value) == 1 && is.finite(value), logical(1)))) {
      embedded$source <- "embedded"
      return(normalize_scanpath_geometry(embedded))
    }
  }

  if (!all(c(row_col, col_col) %in% names(events))) {
    stop("Events must contain row and col columns for scanpath geometry detection.", call. = FALSE)
  }

  valid_positions <- events |>
    dplyr::filter(is.finite(.data[[row_col]]), is.finite(.data[[col_col]]))

  if (nrow(valid_positions) == 0) {
    stop("Cannot detect scanpath geometry without finite row/col positions.", call. = FALSE)
  }

  normalize_scanpath_geometry(list(
    row_min = floor(min(valid_positions[[row_col]], na.rm = TRUE)),
    row_max = ceiling(max(valid_positions[[row_col]], na.rm = TRUE)),
    col_min = floor(min(valid_positions[[col_col]], na.rm = TRUE)),
    col_max = ceiling(max(valid_positions[[col_col]], na.rm = TRUE)),
    source = "auto"
  ))
}

scanpath_board_x <- function(col, geometry) {
  -0.5 + ((col - geometry$col_min) + 0.5) / geometry$n_cols
}

scanpath_board_y <- function(row, geometry) {
  0.5 - ((row - geometry$row_min) + 0.5) / geometry$n_rows
}

resolve_scanpath_id_cols <- function(events, scanpath_id_cols = NULL) {
  if (is.null(scanpath_id_cols)) {
    scanpath_id_cols <- intersect(c("participant", "device", "trial"), names(events))
  }
  missing <- setdiff(scanpath_id_cols, names(events))
  if (length(missing) > 0) {
    stop("Scanpath ID columns are missing from events: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (length(scanpath_id_cols) == 0) {
    stop("At least one scanpath ID column is required.", call. = FALSE)
  }
  scanpath_id_cols
}

build_scanpath_ids <- function(events, scanpath_id_cols) {
  parts <- lapply(scanpath_id_cols, function(col) {
    values <- events[[col]]
    if (identical(col, "participant")) {
      paste0("p", values)
    } else if (identical(col, "trial")) {
      trial_values <- suppressWarnings(as.integer(values))
      ifelse(is.na(trial_values), paste0("t", values), paste0("t", sprintf("%02d", trial_values)))
    } else {
      paste0(col, values)
    }
  })

  do.call(paste, c(parts, sep = "_"))
}

ensure_mclust_attached <- function() {
  if (!"package:mclust" %in% search()) {
    suppressPackageStartupMessages(base::attachNamespace("mclust"))
  }
  invisible(TRUE)
}

build_scanpaths <- function(
    events,
    trial_metrics,
    analysis_id,
    output_dir,
    analysis_filter = rep(TRUE, nrow(events)),
    click_levels = default_click_levels(),
    duration_normalization = NULL,
    modulator = NULL,
    viewing_distance = NULL,
    unit_size = NULL,
    mds_dimension_candidates = NULL,
    mds_stress_acceptability_threshold = NULL,
    candidate_cluster_counts = NULL,
    geometry = NULL,
    scanpath_id_cols = NULL) {
  analysis_dir <- file.path(output_dir, analysis_id)
  dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

  scanpath_geometry <- detect_scanpath_geometry(events, geometry = geometry)
  scanpath_id_cols <- resolve_scanpath_id_cols(events, scanpath_id_cols)

  included_events <- events |>
    dplyr::filter(analysis_filter)

  included_events$scanpath_id <- build_scanpath_ids(included_events, scanpath_id_cols)

  included_events <- included_events |>
    dplyr::mutate(
      click_event = factor(.data$click_event, levels = click_levels),
      cell_id = paste(.data$row, .data$col, sep = "_"),
      valid_event = is.finite(.data$row) & is.finite(.data$col) & is.finite(.data$time),
      board_x = scanpath_board_x(.data$col, scanpath_geometry),
      board_y = scanpath_board_y(.data$row, scanpath_geometry),
      geometry_row_min = scanpath_geometry$row_min,
      geometry_row_max = scanpath_geometry$row_max,
      geometry_col_min = scanpath_geometry$col_min,
      geometry_col_max = scanpath_geometry$col_max,
      geometry_n_rows = scanpath_geometry$n_rows,
      geometry_n_cols = scanpath_geometry$n_cols,
      geometry_source = scanpath_geometry$source
    )

  scanpath_sequences <- included_events |>
    dplyr::arrange(.data$participant, .data$trial, .data$eventn, .data$time) |>
    dplyr::group_by(.data$scanpath_id) |>
    dplyr::mutate(consecutive_duplicate = .data$cell_id == dplyr::lag(.data$cell_id, default = "")) |>
    dplyr::filter(.data$valid_event, !.data$consecutive_duplicate) |>
    dplyr::mutate(
      event_order = dplyr::row_number(),
      raw_next_time = dplyr::lead(.data$time),
      raw_duration = .data$raw_next_time - .data$time,
      positive_intervisit = dplyr::if_else(.data$raw_duration > 0, .data$raw_duration, NA_real_),
      median_intervisit = median_or_na(.data$positive_intervisit),
      trial_end_remaining = pmax(max(.data$dur, na.rm = TRUE) - .data$time, 0.001),
      duration_rule = dplyr::case_when(
        is.finite(.data$raw_duration) & .data$raw_duration > 0 ~ "next valid event",
        is.finite(.data$median_intervisit) & .data$median_intervisit > 0 ~ "trial median imputation",
        TRUE ~ "trial-end fallback"
      ),
      duration = dplyr::case_when(
        .data$duration_rule == "next valid event" ~ .data$raw_duration,
        .data$duration_rule == "trial median imputation" ~ .data$median_intervisit,
        TRUE ~ .data$trial_end_remaining
      ),
      next_col = dplyr::lead(.data$col),
      next_row = dplyr::lead(.data$row)
    ) |>
    dplyr::ungroup()

  retained_cols <- intersect(
    c(
      "scanpath_id", "participant", "pid", "trial",
      "device", "condition", "source_condition_label", "collapsed_condition", "cndnum",
      "high_fruit", "low_fruit", "fxfrst", "redfrt", "event_order", "eventn",
      "time", "dur", "tree", "row", "col", "board_x", "board_y", "cell_id",
      "geometry_row_min", "geometry_row_max", "geometry_col_min", "geometry_col_max",
      "geometry_n_rows", "geometry_n_cols", "geometry_source",
      "click_event", "consume_event", "consume_value", "score", "score_delta",
      "duration", "duration_rule", "next_col", "next_row"
    ),
    names(scanpath_sequences)
  )

  scanpath_sequences <- scanpath_sequences |>
    dplyr::select(dplyr::all_of(retained_cols))

  metadata_summary <- list(
    participant = dplyr::first,
    trial = dplyr::first,
    events = length,
    unique_cells = function(x) length(unique(x)),
    total_intervisit_time = sum,
    median_intervisit_time = stats::median,
    final_duration_rule = dplyr::last
  )

  scanpath_metadata <- scanpath_sequences |>
    dplyr::group_by(.data$scanpath_id) |>
    dplyr::summarise(
      participant = dplyr::first(.data$participant),
      pid = if ("pid" %in% names(scanpath_sequences)) dplyr::first(.data$pid) else NA_character_,
      trial = dplyr::first(.data$trial),
      device = if ("device" %in% names(scanpath_sequences)) dplyr::first(.data$device) else NA_character_,
      condition = if ("condition" %in% names(scanpath_sequences)) dplyr::first(.data$condition) else NA_character_,
      source_condition_label = if ("source_condition_label" %in% names(scanpath_sequences)) dplyr::first(.data$source_condition_label) else NA_character_,
      collapsed_condition = if ("collapsed_condition" %in% names(scanpath_sequences)) dplyr::first(.data$collapsed_condition) else NA_character_,
      cndnum = if ("cndnum" %in% names(scanpath_sequences)) dplyr::first(.data$cndnum) else NA_integer_,
      high_fruit = if ("high_fruit" %in% names(scanpath_sequences)) dplyr::first(.data$high_fruit) else NA_character_,
      low_fruit = if ("low_fruit" %in% names(scanpath_sequences)) dplyr::first(.data$low_fruit) else NA_character_,
      fxfrst = if ("fxfrst" %in% names(scanpath_sequences)) dplyr::first(.data$fxfrst) else NA,
      redfrt = if ("redfrt" %in% names(scanpath_sequences)) dplyr::first(.data$redfrt) else NA,
      events = dplyr::n(),
      unique_cells = dplyr::n_distinct(.data$cell_id),
      total_intervisit_time = sum(.data$duration),
      median_intervisit_time = stats::median(.data$duration),
      final_duration_rule = dplyr::last(.data$duration_rule),
      first_high_time = min_or_na(.data$time[.data$click_event == "consume high"]),
      .groups = "drop"
    )

  metadata_join_cols <- Reduce(
    intersect,
    list(c("participant", "device", "trial"), names(scanpath_metadata), names(trial_metrics))
  )

  if (length(metadata_join_cols) > 0) {
    scanpath_metadata <- scanpath_metadata |>
      dplyr::left_join(trial_metrics, by = metadata_join_cols)
  }

  scanpath_manifest <- tibble::tibble(
    metric = c(
      "analysis_id",
      "scanpath_package_version",
      "raw_click_rows",
      "scanpath_rows",
      "participant_trials",
      "participants",
      "removed_invalid_or_consecutive_duplicate_rows",
      "geometry_source",
      "geometry_rows",
      "geometry_cols",
      "geometry_row_min",
      "geometry_row_max",
      "geometry_col_min",
      "geometry_col_max",
      "scanpath_id_columns",
      "metadata_join_columns"
    ),
    value = c(
      analysis_id,
      as.character(utils::packageVersion("scanpath")),
      as.character(nrow(included_events)),
      as.character(nrow(scanpath_sequences)),
      as.character(dplyr::n_distinct(scanpath_sequences$scanpath_id)),
      as.character(dplyr::n_distinct(scanpath_sequences$participant)),
      as.character(nrow(included_events) - nrow(scanpath_sequences)),
      scanpath_geometry$source,
      as.character(scanpath_geometry$n_rows),
      as.character(scanpath_geometry$n_cols),
      as.character(scanpath_geometry$row_min),
      as.character(scanpath_geometry$row_max),
      as.character(scanpath_geometry$col_min),
      as.character(scanpath_geometry$col_max),
      paste(scanpath_id_cols, collapse = ", "),
      paste(metadata_join_cols, collapse = ", ")
    )
  )

  optional_manifest <- tibble::tibble(
    metric = c(
      "duration_normalization",
      "modulator",
      "viewing_distance_screen_heights",
      "unit_size_screen_heights",
      "mds_dimension_candidates",
      "mds_stress_acceptability_threshold",
      "candidate_cluster_range"
    ),
    value = c(
      if (is.null(duration_normalization)) NA_character_ else as.character(duration_normalization),
      if (is.null(modulator)) NA_character_ else as.character(modulator),
      if (is.null(viewing_distance)) NA_character_ else as.character(viewing_distance),
      if (is.null(unit_size)) NA_character_ else as.character(unit_size),
      if (is.null(mds_dimension_candidates)) NA_character_ else paste(mds_dimension_candidates, collapse = ", "),
      if (is.null(mds_stress_acceptability_threshold)) NA_character_ else as.character(mds_stress_acceptability_threshold),
      if (is.null(candidate_cluster_counts)) NA_character_ else paste(range(candidate_cluster_counts), collapse = "-")
    )
  ) |>
    dplyr::filter(!is.na(.data$value))

  scanpath_manifest <- dplyr::bind_rows(scanpath_manifest, optional_manifest)

  readr::write_csv(scanpath_sequences, file.path(analysis_dir, "scanpath_sequences.csv"))
  readr::write_csv(scanpath_metadata, file.path(analysis_dir, "scanpath_metadata.csv"))
  readr::write_csv(scanpath_manifest, file.path(analysis_dir, "scanpath_manifest.csv"))

  list(
    analysis_dir = analysis_dir,
    sequences = scanpath_sequences,
    metadata = scanpath_metadata,
    manifest = scanpath_manifest
  )
}

compute_scasim_distance <- function(
    scanpath_bundle,
    center_x = 0,
    center_y = 0,
    viewing_distance = 2,
    unit_size = 1,
    modulator = 0.83,
    normalize = "durations") {
  analysis_dir <- scanpath_bundle$analysis_dir
  distance_path <- file.path(analysis_dir, "scasim_distance_matrix.rds")

  scasim_input <- scanpath_bundle$sequences |>
    dplyr::transmute(
      scanpath_id = .data$scanpath_id,
      duration = .data$duration,
      x = .data$board_x,
      y = .data$board_y
    ) |>
    as.data.frame()

  matrix_ok <- FALSE
  if (file.exists(distance_path)) {
    scasim_distance_matrix <- readRDS(distance_path)
    matrix_ok <- setequal(rownames(scasim_distance_matrix), scanpath_bundle$metadata$scanpath_id)
  }

  if (!matrix_ok) {
    scasim_distance_matrix <- scanpath::scasim(
      scasim_input,
      duration ~ x + y | scanpath_id,
      center_x,
      center_y,
      viewing_distance,
      unit_size,
      modulator = modulator,
      normalize = normalize
    )
    saveRDS(scasim_distance_matrix, distance_path)
  }

  distance_audit <- tibble::tibble(
    rows = nrow(scasim_distance_matrix),
    columns = ncol(scasim_distance_matrix),
    scanpaths_in_metadata = dplyr::n_distinct(scanpath_bundle$metadata$scanpath_id),
    rownames_match_metadata = setequal(rownames(scasim_distance_matrix), scanpath_bundle$metadata$scanpath_id),
    symmetric = isTRUE(all.equal(scasim_distance_matrix, t(scasim_distance_matrix))),
    zero_diagonal = all(diag(scasim_distance_matrix) == 0),
    finite_values = all(is.finite(scasim_distance_matrix)),
    minimum = min(scasim_distance_matrix),
    median = stats::median(scasim_distance_matrix),
    maximum = max(scasim_distance_matrix)
  )

  readr::write_csv(distance_audit, file.path(analysis_dir, "scasim_distance_audit.csv"))

  list(matrix = scasim_distance_matrix, audit = distance_audit)
}

mclust_bic_table <- function(fit) {
  as.data.frame(as.table(fit$BIC), stringsAsFactors = FALSE) |>
    tibble::as_tibble() |>
    dplyr::rename(cluster_count = .data$Var1, model_name = .data$Var2, bic = .data$Freq) |>
    dplyr::mutate(cluster_count = as.integer(as.character(.data$cluster_count))) |>
    dplyr::filter(is.finite(.data$bic))
}

fit_scanpath_dimension_candidate <- function(dist_obj, dimensions, candidate_cluster_counts = 1:10) {
  initial_map <- tryCatch(
    stats::cmdscale(dist_obj, k = dimensions),
    error = function(error) error
  )

  if (!is.matrix(initial_map) || ncol(initial_map) < dimensions) {
    return(list(
      summary = tibble::tibble(
        mds_dimensions = dimensions,
        stress = NA_real_,
        selected_clusters = NA_integer_,
        model_name = NA_character_,
        bic = NA_real_,
        mean_uncertainty = NA_real_,
        smallest_cluster = NA_integer_,
        largest_cluster = NA_integer_,
        status = "cmdscale initialization failed",
        message = if (inherits(initial_map, "error")) conditionMessage(initial_map) else "cmdscale returned too few dimensions"
      ),
      coordinates = NULL,
      fit = NULL
    ))
  }

  iso_fit <- tryCatch(
    MASS::isoMDS(dist_obj, y = initial_map, k = dimensions, trace = FALSE),
    error = function(error) error
  )

  if (inherits(iso_fit, "error")) {
    return(list(
      summary = tibble::tibble(
        mds_dimensions = dimensions,
        stress = NA_real_,
        selected_clusters = NA_integer_,
        model_name = NA_character_,
        bic = NA_real_,
        mean_uncertainty = NA_real_,
        smallest_cluster = NA_integer_,
        largest_cluster = NA_integer_,
        status = "isoMDS failed",
        message = conditionMessage(iso_fit)
      ),
      coordinates = NULL,
      fit = NULL
    ))
  }

  coordinates <- iso_fit$points[, seq_len(dimensions), drop = FALSE]
  colnames(coordinates) <- paste0("MDS", seq_len(dimensions))
  rownames(coordinates) <- rownames(initial_map)

  mclust_fit <- tryCatch(
    {
      ensure_mclust_attached()
      mclust::Mclust(
        coordinates,
        G = candidate_cluster_counts,
        verbose = FALSE
      )
    },
    error = function(error) error
  )

  if (inherits(mclust_fit, "error")) {
    return(list(
      summary = tibble::tibble(
        mds_dimensions = dimensions,
        stress = iso_fit$stress,
        selected_clusters = NA_integer_,
        model_name = NA_character_,
        bic = NA_real_,
        mean_uncertainty = NA_real_,
        smallest_cluster = NA_integer_,
        largest_cluster = NA_integer_,
        status = "mclust failed",
        message = conditionMessage(mclust_fit)
      ),
      coordinates = coordinates,
      fit = NULL
    ))
  }

  cluster_sizes <- table(mclust_fit$classification)

  list(
    summary = tibble::tibble(
      mds_dimensions = dimensions,
      stress = iso_fit$stress,
      selected_clusters = mclust_fit$G,
      model_name = mclust_fit$modelName,
      bic = max(mclust_fit$BIC, na.rm = TRUE),
      mean_uncertainty = mean(mclust_fit$uncertainty),
      smallest_cluster = min(cluster_sizes),
      largest_cluster = max(cluster_sizes),
      status = "ok",
      message = ""
    ),
    coordinates = coordinates,
    fit = mclust_fit
  )
}

select_scanpath_mds_dimension <- function(
    sweep_summary,
    stress_threshold = 10,
    candidate_cluster_counts = 1:10) {
  selectable <- sweep_summary |>
    dplyr::filter(
      .data$status == "ok",
      is.finite(.data$stress),
      !is.na(.data$selected_clusters)
    )

  if (nrow(selectable) == 0) {
    stop("No successful MDS/Mclust candidate solutions were available for selection.")
  }

  eligible <- selectable |>
    dplyr::filter(.data$stress <= stress_threshold)

  if (nrow(eligible) > 0) {
    selected <- eligible |>
      dplyr::arrange(.data$selected_clusters, .data$stress, .data$mds_dimensions) |>
      dplyr::slice_head(n = 1)
    selection_rule <- paste0(
      "Among finite candidate maps with isoMDS stress <= ",
      stress_threshold,
      ", select the map with the fewest BIC-selected clusters, then lower stress, then fewer dimensions."
    )
    selection_warning <- ""
  } else {
    selected <- selectable |>
      dplyr::arrange(.data$stress, .data$selected_clusters, .data$mds_dimensions) |>
      dplyr::slice_head(n = 1)
    selection_rule <- paste0(
      "No finite candidate map had isoMDS stress <= ",
      stress_threshold,
      "; selected the lowest-stress available map, then fewer clusters, then fewer dimensions."
    )
    selection_warning <- paste0("No candidate reached the stress <= ", stress_threshold, " threshold.")
  }

  selected |>
    dplyr::transmute(
      selected_mds_dimensions = .data$mds_dimensions,
      selected_stress = .data$stress,
      selected_clusters = .data$selected_clusters,
      selected_model_name = .data$model_name,
      selected_bic = .data$bic,
      selected_mean_uncertainty = .data$mean_uncertainty,
      selected_smallest_cluster = .data$smallest_cluster,
      selected_largest_cluster = .data$largest_cluster,
      stress_threshold = stress_threshold,
      hit_cluster_cap = .data$selected_clusters == max(candidate_cluster_counts),
      selection_rule = selection_rule,
      selection_warning = selection_warning
    )
}

fit_scanpath_dimension_sweep <- function(
    distance_bundle,
    scanpath_bundle,
    mds_dimension_candidates = 2:10,
    stress_threshold = 10,
    candidate_cluster_counts = 1:10) {
  analysis_dir <- scanpath_bundle$analysis_dir
  dist_obj <- stats::as.dist(distance_bundle$matrix)
  available_dimensions <- mds_dimension_candidates[
    mds_dimension_candidates <= nrow(distance_bundle$matrix) - 1
  ]

  if (length(available_dimensions) == 0) {
    stop("No MDS dimension candidates were available for scanpath clustering.")
  }

  candidate_results <- stats::setNames(available_dimensions, as.character(available_dimensions)) |>
    purrr::map(~ fit_scanpath_dimension_candidate(dist_obj, .x, candidate_cluster_counts))

  sweep_summary <- purrr::map_dfr(candidate_results, "summary") |>
    dplyr::mutate(
      hit_cluster_cap = .data$selected_clusters == max(candidate_cluster_counts),
      stress_acceptable = is.finite(.data$stress) & .data$stress <= stress_threshold
    )

  selection <- select_scanpath_mds_dimension(
    sweep_summary,
    stress_threshold = stress_threshold,
    candidate_cluster_counts = candidate_cluster_counts
  )
  sweep_summary <- sweep_summary |>
    dplyr::mutate(selected_solution = .data$mds_dimensions == selection$selected_mds_dimensions[[1]])

  bic_by_model <- purrr::imap_dfr(candidate_results, function(result, dimension_name) {
    if (is.null(result$fit)) {
      return(tibble::tibble())
    }
    mclust_bic_table(result$fit) |>
      dplyr::mutate(mds_dimensions = as.integer(dimension_name), .before = 1)
  })

  readr::write_csv(sweep_summary, file.path(analysis_dir, "mds_dimension_sensitivity.csv"))
  readr::write_csv(selection, file.path(analysis_dir, "mds_dimension_selection.csv"))
  readr::write_csv(bic_by_model, file.path(analysis_dir, "mds_dimension_mclust_bic_by_model.csv"))

  list(
    summary = sweep_summary,
    selection = selection,
    candidate_results = candidate_results
  )
}

fit_scanpath_clusters <- function(dimension_sweep, scanpath_bundle, candidate_cluster_counts = 1:10) {
  analysis_dir <- scanpath_bundle$analysis_dir
  selected_dimensions <- dimension_sweep$selection$selected_mds_dimensions[[1]]
  selected_result <- dimension_sweep$candidate_results[[as.character(selected_dimensions)]]

  if (is.null(selected_result) || is.null(selected_result$fit) || is.null(selected_result$coordinates)) {
    stop("The selected MDS/Mclust result was not available for downstream clustering.")
  }

  mds_matrix <- selected_result$coordinates
  mds_cols <- colnames(mds_matrix)
  rownames(mds_matrix) <- rownames(selected_result$coordinates)

  mds_embedding <- tibble::tibble(scanpath_id = rownames(mds_matrix)) |>
    dplyr::bind_cols(tibble::as_tibble(mds_matrix, .name_repair = "minimal")) |>
    stats::setNames(c("scanpath_id", mds_cols)) |>
    dplyr::left_join(scanpath_bundle$metadata, by = "scanpath_id")

  mds_diagnostics <- dimension_sweep$summary |>
    dplyr::select(
      .data$mds_dimensions,
      .data$stress,
      .data$selected_clusters,
      .data$model_name,
      .data$bic,
      .data$mean_uncertainty,
      .data$smallest_cluster,
      .data$largest_cluster,
      .data$stress_acceptable,
      .data$hit_cluster_cap,
      .data$selected_solution,
      .data$status,
      .data$message
    ) |>
    dplyr::arrange(.data$mds_dimensions)

  mclust_fit <- selected_result$fit

  assignments <- tibble::tibble(
    scanpath_id = rownames(mds_matrix),
    cluster_id = factor(mclust_fit$classification)
  ) |>
    dplyr::left_join(mds_embedding, by = "scanpath_id") |>
    dplyr::mutate(
      participant = factor(.data$participant),
      collapsed_condition = factor(.data$collapsed_condition, levels = default_condition_levels())
    )

  cluster_sizes <- assignments |>
    dplyr::count(.data$cluster_id, name = "scanpaths") |>
    dplyr::arrange(dplyr::desc(.data$scanpaths), .data$cluster_id)

  mclust_summary <- tibble::tibble(
    selected_mds_dimensions = selected_dimensions,
    selected_stress = dimension_sweep$selection$selected_stress[[1]],
    stress_threshold = dimension_sweep$selection$stress_threshold[[1]],
    selected_clusters = mclust_fit$G,
    model_name = mclust_fit$modelName,
    bic = max(mclust_fit$BIC, na.rm = TRUE),
    mean_uncertainty = mean(mclust_fit$uncertainty),
    smallest_cluster = min(cluster_sizes$scanpaths),
    largest_cluster = max(cluster_sizes$scanpaths),
    hit_cluster_cap = mclust_fit$G == max(candidate_cluster_counts),
    selection_rule = dimension_sweep$selection$selection_rule[[1]],
    selection_warning = dimension_sweep$selection$selection_warning[[1]]
  )

  readr::write_csv(mds_embedding, file.path(analysis_dir, "mds_embedding.csv"))
  readr::write_csv(mds_diagnostics, file.path(analysis_dir, "mds_diagnostics_by_dimension.csv"))
  readr::write_csv(assignments, file.path(analysis_dir, "mclust_assignments.csv"))
  readr::write_csv(cluster_sizes, file.path(analysis_dir, "mclust_cluster_sizes.csv"))
  readr::write_csv(mclust_summary, file.path(analysis_dir, "mclust_summary.csv"))
  readr::write_csv(mclust_bic_table(mclust_fit), file.path(analysis_dir, "mclust_bic_by_model.csv"))

  list(
    mds_cols = mds_cols,
    mds_embedding = mds_embedding,
    mds_diagnostics = mds_diagnostics,
    dimension_sweep = dimension_sweep,
    fit = mclust_fit,
    assignments = assignments,
    cluster_sizes = cluster_sizes,
    summary = mclust_summary
  )
}
