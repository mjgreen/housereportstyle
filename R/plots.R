theme_report <- function(base_size = 11) {
  jtools::theme_apa(
    legend.pos = "bottom",
    legend.use.title = TRUE,
    legend.font.size = base_size,
    x.font.size = base_size,
    y.font.size = base_size,
    facet.title.size = base_size
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(size = max(base_size - 2, 8), colour = "grey25")
    )
}

save_report_plot <- function(plot, filename, plot_dir, width = 8, height = 5.5, dpi = 180) {
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(file.path(plot_dir, filename), plot, width = width, height = height, dpi = dpi)
  invisible(plot)
}

figure_note_aside <- function(note) {
  paste0("::: {.aside}\n_Note._ ", note, "\n:::\n")
}

stack_bottom_legends_if_wide <- function(plot, plot_width, max_fraction = 0.96) {
  horizontal_plot <- plot +
    ggplot2::theme(legend.position = "bottom", legend.box = "horizontal")
  plot_grob <- ggplot2::ggplotGrob(horizontal_plot)
  guide_index <- which(grepl("^guide-box", plot_grob$layout$name))

  if (length(guide_index) == 0) {
    return(horizontal_plot)
  }

  legend_width <- grid::convertWidth(
    grid::grobWidth(plot_grob$grobs[[guide_index[[1]]]]),
    "in",
    valueOnly = TRUE
  )

  if (is.finite(legend_width) && legend_width > plot_width * max_fraction) {
    horizontal_plot + ggplot2::theme(legend.box = "vertical")
  } else {
    horizontal_plot
  }
}

