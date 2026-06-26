test_that("MDS selection prefers fewer clusters among stress-acceptable maps", {
  sweep <- tibble::tibble(
    mds_dimensions = 2:6,
    stress = c(30, 12, 9, 7, 6),
    selected_clusters = c(4, 4, 7, 5, 5),
    model_name = "EEE",
    bic = 1,
    mean_uncertainty = 0.1,
    smallest_cluster = 10,
    largest_cluster = 50,
    status = "ok",
    message = ""
  )

  selected <- select_scanpath_mds_dimension(sweep, stress_threshold = 10, candidate_cluster_counts = 1:10)

  expect_equal(selected$selected_mds_dimensions, 6)
  expect_equal(selected$selected_clusters, 5)
  expect_false(selected$hit_cluster_cap)
})

test_that("MDS selection falls back to lowest stress if no map is acceptable", {
  sweep <- tibble::tibble(
    mds_dimensions = 2:4,
    stress = c(30, 20, 15),
    selected_clusters = c(4, 3, 5),
    model_name = "EEE",
    bic = 1,
    mean_uncertainty = 0.1,
    smallest_cluster = 10,
    largest_cluster = 50,
    status = "ok",
    message = ""
  )

  selected <- select_scanpath_mds_dimension(sweep, stress_threshold = 10, candidate_cluster_counts = 1:10)

  expect_equal(selected$selected_mds_dimensions, 4)
  expect_match(selected$selection_warning, "No candidate")
})

test_that("MDS diagnostic plot is a ggplot", {
  sweep <- tibble::tibble(
    mds_dimensions = 2:10,
    stress = seq(20, 8, length.out = 9),
    selected_clusters = c(4, 5, 5, 6, 6, 7, 7, 8, 8),
    model_name = "EEE",
    bic = 1,
    mean_uncertainty = 0.1,
    smallest_cluster = 10,
    largest_cluster = 50,
    status = "ok",
    message = ""
  )
  selected <- select_scanpath_mds_dimension(sweep)
  plot <- plot_mds_dimension_selection(list(summary = sweep, selection = selected))
  expect_s3_class(plot, "ggplot")
})

test_that("scanpath dimension candidate can fit mclust from package namespace", {
  set.seed(42)
  coordinates <- rbind(
    matrix(rnorm(40, mean = -1, sd = 0.15), ncol = 2),
    matrix(rnorm(40, mean = 1, sd = 0.15), ncol = 2)
  )
  rownames(coordinates) <- paste0("s", seq_len(nrow(coordinates)))
  dist_obj <- stats::dist(coordinates)

  result <- fit_scanpath_dimension_candidate(dist_obj, dimensions = 2, candidate_cluster_counts = 1:3)

  expect_equal(result$summary$status, "ok")
  expect_true(is.finite(result$summary$stress))
  expect_true(result$summary$selected_clusters %in% 1:3)
})
