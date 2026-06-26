test_that("scanpath geometry detection supports 24 x 24 zero-based layouts", {
  events <- tibble::tibble(
    row = c(0, 23),
    col = c(0, 23)
  )

  geometry <- detect_scanpath_geometry(events)

  expect_equal(geometry$row_min, 0L)
  expect_equal(geometry$row_max, 23L)
  expect_equal(geometry$col_min, 0L)
  expect_equal(geometry$col_max, 23L)
  expect_equal(geometry$n_rows, 24L)
  expect_equal(geometry$n_cols, 24L)
  expect_equal(geometry$source, "auto")
})

test_that("scanpath geometry detection supports 9 x 16 one-based layouts", {
  events <- tibble::tibble(
    row = c(1, 9),
    col = c(1, 16)
  )

  geometry <- detect_scanpath_geometry(events)

  expect_equal(geometry$row_min, 1L)
  expect_equal(geometry$row_max, 9L)
  expect_equal(geometry$col_min, 1L)
  expect_equal(geometry$col_max, 16L)
  expect_equal(geometry$n_rows, 9L)
  expect_equal(geometry$n_cols, 16L)
})

test_that("build_scanpaths maps board coordinates from detected geometry", {
  events <- tibble::tibble(
    participant = c("P1", "P1", "P1", "P1"),
    pid = c("P1", "P1", "P1", "P1"),
    trial = c(1, 1, 1, 1),
    eventn = c(1, 2, 3, 4),
    time = c(0, 1, 2, 3),
    dur = c(4, 4, 4, 4),
    row = c(1, 1, 9, 9),
    col = c(1, 16, 16, 1),
    click_event = c("visit", "visit", "visit", "visit"),
    collapsed_condition = c("same trees / same fruit", "same trees / same fruit", "same trees / same fruit", "same trees / same fruit")
  )
  trial_metrics <- tibble::tibble(
    participant = "P1",
    trial = 1,
    trial_metric = 1
  )

  bundle <- build_scanpaths(
    events = events,
    trial_metrics = trial_metrics,
    analysis_id = "geometry-test",
    output_dir = tempdir(),
    click_levels = "visit"
  )

  expect_equal(unique(bundle$sequences$geometry_n_rows), 9)
  expect_equal(unique(bundle$sequences$geometry_n_cols), 16)
  expect_equal(bundle$sequences$board_x[[1]], -0.5 + 0.5 / 16)
  expect_equal(bundle$sequences$board_x[[2]], -0.5 + 15.5 / 16)
  expect_equal(bundle$sequences$board_y[[1]], 0.5 - 0.5 / 9)
  expect_equal(bundle$sequences$board_y[[3]], 0.5 - 8.5 / 9)
  expect_true(any(bundle$manifest$metric == "geometry_rows" & bundle$manifest$value == "9"))
  expect_true(any(bundle$manifest$metric == "geometry_cols" & bundle$manifest$value == "16"))
})

test_that("build_scanpaths includes device in scanpath identity when present", {
  events <- tibble::tibble(
    participant = c("P1", "P1", "P1", "P1"),
    device = c("mouse", "mouse", "eyes", "eyes"),
    trial = c(1, 1, 1, 1),
    eventn = c(1, 2, 1, 2),
    time = c(0, 1, 0, 1),
    dur = c(2, 2, 2, 2),
    row = c(1, 2, 1, 2),
    col = c(1, 2, 1, 2),
    click_event = c("visit", "visit", "visit", "visit")
  )
  trial_metrics <- tibble::tibble(
    participant = c("P1", "P1"),
    device = c("mouse", "eyes"),
    trial = c(1, 1),
    completion_status = c("complete", "timeout")
  )

  bundle <- build_scanpaths(
    events = events,
    trial_metrics = trial_metrics,
    analysis_id = "device-identity-test",
    output_dir = tempdir(),
    click_levels = "visit"
  )

  expect_equal(nrow(bundle$metadata), 2)
  expect_setequal(bundle$metadata$device, c("mouse", "eyes"))
  expect_setequal(bundle$metadata$completion_status, c("complete", "timeout"))
  expect_true(any(bundle$manifest$metric == "scanpath_id_columns" & bundle$manifest$value == "participant, device, trial"))
  expect_true(any(bundle$manifest$metric == "metadata_join_columns" & bundle$manifest$value == "participant, device, trial"))
})

test_that("scanpath cluster plots recover embedded geometry", {
  sequences <- tibble::tibble(
    scanpath_id = c("a", "a", "b", "b"),
    event_order = c(1, 2, 1, 2),
    row = c(1, 9, 1, 9),
    col = c(1, 16, 16, 1),
    geometry_row_min = 1,
    geometry_row_max = 9,
    geometry_col_min = 1,
    geometry_col_max = 16,
    geometry_n_rows = 9,
    geometry_n_cols = 16,
    geometry_source = "embedded"
  )
  assignments <- tibble::tibble(
    scanpath_id = c("a", "b"),
    cluster_id = factor(c(1, 2))
  )
  cluster_sizes <- tibble::tibble(
    cluster_id = factor(c(1, 2)),
    scanpaths = c(1L, 1L)
  )

  plot <- plot_scanpath_lines_by_cluster(assignments, sequences, cluster_sizes)

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$scales$get_scales("x")$limits, c(0.5, 16.5))
  expect_equal(abs(plot$scales$get_scales("y")$limits), c(9.5, 0.5))
})
