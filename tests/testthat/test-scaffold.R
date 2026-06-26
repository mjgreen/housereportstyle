test_that("create_report scaffolds a report and generated directories", {
  path <- file.path(tempdir(), paste0("housereportstyle-", as.integer(stats::runif(1, 1, 1e8))))

  result <- create_report(path, report_id = "example-report", title = "Example Report")

  expect_true(file.exists(file.path(path, "report.qmd")))
  expect_true(file.exists(file.path(path, "report-style.css")))
  expect_true(dir.exists(file.path(path, "generated")))
  expect_true(dir.exists(file.path(path, "generated", "plots")))
  expect_equal(result$template, "scientific-html")
  qmd <- paste(readLines(file.path(path, "report.qmd"), warn = FALSE), collapse = "\n")
  css <- paste(readLines(file.path(path, "report-style.css"), warn = FALSE), collapse = "\n")
  expect_match(qmd, "Example Report")
  expect_match(qmd, "toc-location: right")
  expect_match(qmd, "dark: darkly")
  expect_match(qmd, "```\\{r tbl-data-input\\}")
  expect_match(qmd, "tbl-cap:")
  expect_match(qmd, "@tbl-data-input")
  expect_false(grepl("label = \"Table 1\"", qmd, fixed = TRUE))
  expect_match(css, "quarto-color-scheme-toggle")
  expect_match(css, "content: \"Dark mode\"")
  expect_match(css, "min-height: 2[.]55rem")
})

test_that("create_report can use scanpath template", {
  path <- file.path(tempdir(), paste0("housereportstyle-scanpath-", as.integer(stats::runif(1, 1, 1e8))))

  create_report(path, report_id = "scanpath-report", title = "Scanpath Report", template = "scanpath-strategy")

  qmd <- paste(readLines(file.path(path, "report.qmd"), warn = FALSE), collapse = "\n")
  expect_match(qmd, "Scanpath strategy report scaffold")
  expect_match(qmd, "```\\{r tbl-scanpath-manifest\\}")
  expect_match(qmd, "@tbl-scanpath-manifest")
  expect_match(qmd, "fig-mds-dimension-selection")
})
