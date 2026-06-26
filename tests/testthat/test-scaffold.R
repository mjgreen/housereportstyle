test_that("create_report scaffolds a report and generated directories", {
  path <- file.path(tempdir(), paste0("housereportstyle-", as.integer(stats::runif(1, 1, 1e8))))

  result <- create_report(path, report_id = "example-report", title = "Example Report")

  expect_true(file.exists(file.path(path, "report.qmd")))
  expect_true(file.exists(file.path(path, "report-style.css")))
  expect_true(dir.exists(file.path(path, "generated")))
  expect_true(dir.exists(file.path(path, "generated", "plots")))
  expect_equal(result$template, "scientific-html")
  expect_match(paste(readLines(file.path(path, "report.qmd"), warn = FALSE), collapse = "\n"), "Example Report")
  expect_match(paste(readLines(file.path(path, "report.qmd"), warn = FALSE), collapse = "\n"), "toc-location: left")
  expect_match(paste(readLines(file.path(path, "report.qmd"), warn = FALSE), collapse = "\n"), "dark: darkly")
})

test_that("create_report can use scanpath template", {
  path <- file.path(tempdir(), paste0("housereportstyle-scanpath-", as.integer(stats::runif(1, 1, 1e8))))

  create_report(path, report_id = "scanpath-report", title = "Scanpath Report", template = "scanpath-strategy")

  qmd <- paste(readLines(file.path(path, "report.qmd"), warn = FALSE), collapse = "\n")
  expect_match(qmd, "Scanpath strategy report scaffold")
  expect_match(qmd, "fig-mds-dimension-selection")
})
