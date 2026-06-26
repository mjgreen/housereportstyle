scanpath_overrep_palette <- function(levels = c(default_condition_levels(), "No clear over-representation")) {
  condition_levels <- setdiff(levels, "No clear over-representation")
  condition_colours <- scales::hue_pal()(length(condition_levels))
  names(condition_colours) <- condition_levels
  c(condition_colours, "No clear over-representation" = "white")[levels]
}

theme_scanpath_cluster <- function(base_size = 12, show_legend = TRUE) {
  theme_report(base_size = base_size) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = ggplot2::element_rect(fill = NA, colour = "black", linewidth = 0.5),
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white", colour = "grey70", linewidth = 0.5),
      strip.text = ggplot2::element_text(size = max(base_size - 1.5, 8), face = "bold", colour = "black"),
      panel.spacing = grid::unit(0.7, "lines"),
      legend.position = if (show_legend) "bottom" else "none",
      legend.box = "horizontal",
      legend.title = ggplot2::element_text(face = "bold")
    )
}

scanpath_residual_alpha <- function(
    residual,
    overrepresentation,
    threshold = 1.96,
    min_alpha = 0.25,
    max_alpha = 0.65) {
  is_overrepresented <- overrepresentation != "No clear over-representation" &
    is.finite(residual) &
    residual > threshold

  alpha <- rep(1, length(residual))
  if (!any(is_overrepresented)) {
    return(alpha)
  }

  max_residual <- max(residual[is_overrepresented], na.rm = TRUE)
  if (!is.finite(max_residual) || max_residual <= threshold) {
    alpha[is_overrepresented] <- min_alpha
    return(alpha)
  }

  scaled <- (pmin(residual[is_overrepresented], max_residual) - threshold) /
    (max_residual - threshold)
  alpha[is_overrepresented] <- min_alpha + scaled * (max_alpha - min_alpha)
  alpha
}

scanpath_overrep_strip_theme <- function(strip_data, palette = scanpath_overrep_palette()) {
  strip_data <- strip_data |>
    dplyr::mutate(
      overrepresentation = factor(
        as.character(.data$overrepresentation),
        levels = names(palette)
      ),
      strip_fill = unname(palette[as.character(.data$overrepresentation)]),
      strip_fill = dplyr::if_else(
        as.character(.data$overrepresentation) == "No clear over-representation",
        "white",
        scales::alpha(.data$strip_fill, .data$strip_alpha)
      )
    )

  ggh4x::strip_themed(
    background_x = purrr::map(
      strip_data$strip_fill,
      ~ ggplot2::element_rect(fill = .x, colour = "grey70", linewidth = 0.5)
    ),
    text_x = purrr::map(
      seq_len(nrow(strip_data)),
      ~ ggplot2::element_text(size = 10.5, face = "bold", colour = "black")
    )
  )
}

scanpath_overrep_legend_components <- function(panel_levels, palette = scanpath_overrep_palette()) {
  legend_data <- tibble::tibble(
    cluster_panel = factor(panel_levels[[1]], levels = panel_levels),
    overrepresentation = factor(names(palette), levels = names(palette)),
    x = -0.25,
    y = -0.25
  )

  list(
    ggplot2::geom_point(
      data = legend_data,
      ggplot2::aes(x = .data$x, y = .data$y, fill = .data$overrepresentation),
      inherit.aes = FALSE,
      shape = 22,
      size = 3.8,
      colour = "grey35",
      alpha = 0,
      show.legend = TRUE
    ),
    ggplot2::scale_fill_manual(
      values = palette,
      breaks = names(palette),
      drop = FALSE,
      name = "Strip over-representation"
    ),
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1, size = 4.2)))
  )
}

plot_scanpath_geometry <- function(
    assignments,
    sequences,
    cluster_sizes,
    overrepresentation,
    plot_width = 13.5,
    palette = scanpath_overrep_palette(),
    geometry = NULL) {
  scanpath_geometry <- detect_scanpath_geometry(sequences, geometry = geometry)

  plot_assignments <- assignments |>
    dplyr::select(dplyr::all_of(c("scanpath_id", "cluster_id"))) |>
    dplyr::mutate(
      cluster_id = factor(
        as.character(.data$cluster_id),
        levels = as.character(sort(unique(as.integer(as.character(.data$cluster_id)))))
      )
    ) |>
    dplyr::left_join(cluster_sizes, by = "cluster_id") |>
    dplyr::left_join(
      overrepresentation |>
        dplyr::transmute(
          cluster_id = as.character(.data$cluster_id),
          overrepresentation = .data$overrepresentation,
          largest_standardized_residual = .data$largest_standardized_residual
        ),
      by = "cluster_id"
    ) |>
    dplyr::mutate(
      overrepresentation = tidyr::replace_na(.data$overrepresentation, "No clear over-representation"),
      overrepresentation = factor(.data$overrepresentation, levels = names(palette)),
      largest_standardized_residual = tidyr::replace_na(.data$largest_standardized_residual, NA_real_),
      cluster_panel = paste0("Cluster ", .data$cluster_id, " / n = ", .data$scanpaths)
    )

  panel_order <- plot_assignments |>
    dplyr::arrange(as.integer(as.character(.data$cluster_id))) |>
    dplyr::pull(.data$cluster_panel) |>
    unique()

  plot_assignments <- plot_assignments |>
    dplyr::mutate(
      cluster_panel = factor(.data$cluster_panel, levels = panel_order),
      strip_alpha = scanpath_residual_alpha(
        .data$largest_standardized_residual,
        as.character(.data$overrepresentation)
      )
    )

  strip_data <- plot_assignments |>
    dplyr::distinct(
      .data$cluster_panel,
      .data$overrepresentation,
      .data$largest_standardized_residual,
      .data$strip_alpha
    ) |>
    dplyr::arrange(.data$cluster_panel)

  facet_columns <- min(3, max(1, dplyr::n_distinct(plot_assignments$cluster_id)))

  plot <- sequences |>
    dplyr::inner_join(plot_assignments, by = "scanpath_id") |>
    dplyr::arrange(.data$cluster_id, .data$scanpath_id, .data$event_order) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$col, y = .data$row, group = .data$scanpath_id)) +
    ggplot2::geom_path(linewidth = 0.2, alpha = 0.105, colour = "#17212B", lineend = "round") +
    scanpath_overrep_legend_components(levels(plot_assignments$cluster_panel), palette) +
    ggh4x::facet_wrap2(
      ~ cluster_panel,
      ncol = facet_columns,
      strip = scanpath_overrep_strip_theme(strip_data, palette)
    ) +
    ggplot2::scale_y_reverse(limits = c(scanpath_geometry$row_max + 0.5, scanpath_geometry$row_min - 0.5), breaks = NULL) +
    ggplot2::scale_x_continuous(limits = c(scanpath_geometry$col_min - 0.5, scanpath_geometry$col_max + 0.5), breaks = NULL) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_scanpath_cluster(base_size = 12, show_legend = TRUE)

  stack_bottom_legends_if_wide(plot, plot_width = plot_width)
}

plot_scanpath_lines_by_cluster <- function(assignments, sequences, cluster_sizes, geometry = NULL) {
  scanpath_geometry <- detect_scanpath_geometry(sequences, geometry = geometry)

  plot_assignments <- assignments |>
    dplyr::select(dplyr::all_of(c("scanpath_id", "cluster_id"))) |>
    dplyr::mutate(
      cluster_id = factor(
        as.character(.data$cluster_id),
        levels = as.character(sort(unique(as.integer(as.character(.data$cluster_id)))))
      )
    ) |>
    dplyr::left_join(cluster_sizes, by = "cluster_id") |>
    dplyr::mutate(cluster_panel = paste0("Cluster ", .data$cluster_id, " / n = ", .data$scanpaths))

  panel_order <- plot_assignments |>
    dplyr::arrange(as.integer(as.character(.data$cluster_id))) |>
    dplyr::pull(.data$cluster_panel) |>
    unique()

  plot_assignments <- plot_assignments |>
    dplyr::mutate(cluster_panel = factor(.data$cluster_panel, levels = panel_order))

  facet_columns <- min(3, max(1, dplyr::n_distinct(plot_assignments$cluster_id)))

  sequences |>
    dplyr::inner_join(plot_assignments, by = "scanpath_id") |>
    dplyr::arrange(.data$cluster_id, .data$scanpath_id, .data$event_order) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$col, y = .data$row, group = .data$scanpath_id)) +
    ggplot2::geom_path(linewidth = 0.2, alpha = 0.105, colour = "#17212B", lineend = "round") +
    ggplot2::facet_wrap(~ cluster_panel, ncol = facet_columns) +
    ggplot2::scale_y_reverse(limits = c(scanpath_geometry$row_max + 0.5, scanpath_geometry$row_min - 0.5), breaks = NULL) +
    ggplot2::scale_x_continuous(limits = c(scanpath_geometry$col_min - 0.5, scanpath_geometry$col_max + 0.5), breaks = NULL) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_scanpath_cluster(base_size = 12, show_legend = FALSE)
}

plot_mds_dimension_selection <- function(
    dimension_sweep,
    mds_dimension_candidates = 2:10,
    candidate_cluster_counts = 1:10,
    stress_reference_lines = c(5, 10)) {
  plot_data <- dimension_sweep$summary |>
    dplyr::filter(.data$status == "ok") |>
    dplyr::mutate(selected_clusters = as.numeric(.data$selected_clusters))

  if (nrow(plot_data) == 0) {
    stop("No successful MDS/Mclust candidate solutions were available to plot.")
  }

  selected_dimension <- dimension_sweep$selection$selected_mds_dimensions[[1]]
  stress_max <- max(c(plot_data$stress, stress_reference_lines), na.rm = TRUE)
  stress_max <- ceiling(stress_max + 0.5)
  cluster_min <- min(candidate_cluster_counts)
  cluster_max <- max(candidate_cluster_counts)

  cluster_to_stress <- function(value) {
    ((value - cluster_min) / (cluster_max - cluster_min)) * stress_max
  }

  stress_to_cluster <- function(value) {
    (value / stress_max) * (cluster_max - cluster_min) + cluster_min
  }

  selected_plot_data <- plot_data |>
    dplyr::filter(.data$mds_dimensions == selected_dimension)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$mds_dimensions)) +
    ggplot2::geom_hline(
      yintercept = stress_reference_lines,
      linewidth = 0.45,
      linetype = "dashed",
      colour = "grey45"
    ) +
    ggplot2::geom_vline(
      xintercept = selected_dimension,
      linewidth = 0.55,
      linetype = "dotted",
      colour = "#4477AA"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$stress, linetype = "isoMDS stress"),
      linewidth = 0.65,
      colour = "black"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$stress, shape = "isoMDS stress"),
      size = 3.1,
      colour = "black",
      fill = "black"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = cluster_to_stress(.data$selected_clusters), linetype = "BIC-selected clusters"),
      linewidth = 0.65,
      colour = "black"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = cluster_to_stress(.data$selected_clusters), shape = "BIC-selected clusters"),
      size = 3.4,
      colour = "black",
      fill = "white",
      stroke = 0.9
    ) +
    ggplot2::geom_point(
      data = selected_plot_data,
      ggplot2::aes(x = .data$mds_dimensions, y = .data$stress),
      inherit.aes = FALSE,
      shape = 21,
      size = 4.4,
      colour = "#4477AA",
      fill = "#4477AA"
    ) +
    ggplot2::geom_point(
      data = selected_plot_data,
      ggplot2::aes(
        x = .data$mds_dimensions,
        y = cluster_to_stress(.data$selected_clusters)
      ),
      inherit.aes = FALSE,
      shape = 23,
      size = 4.7,
      colour = "#4477AA",
      fill = "white",
      stroke = 1.1
    ) +
    ggplot2::scale_x_continuous(breaks = mds_dimension_candidates) +
    ggplot2::scale_y_continuous(
      name = "isoMDS stress",
      limits = c(0, stress_max),
      sec.axis = ggplot2::sec_axis(
        ~ stress_to_cluster(.),
        breaks = candidate_cluster_counts,
        name = "BIC-selected clusters"
      )
    ) +
    ggplot2::scale_shape_manual(values = c("isoMDS stress" = 21, "BIC-selected clusters" = 23)) +
    ggplot2::scale_linetype_manual(values = c("isoMDS stress" = "solid", "BIC-selected clusters" = "solid")) +
    ggplot2::labs(
      x = "Number of dimensions in scaled-down map",
      shape = NULL,
      linetype = NULL
    ) +
    theme_report(base_size = 11) +
    ggplot2::theme(
      legend.position = "top",
      legend.box = "horizontal"
    )
}
