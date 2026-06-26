test_that("validate_report_style catches common presentation issues", {
  qmd <- tempfile(fileext = ".qmd")
  writeLines(c(
    "---",
    "title: Bad Example",
    "---",
    "",
    "Figure 1 is hard-coded.",
    "",
    "## Bad `Heading`",
    "",
    "```{r fig-bad}",
    "#| fig-cap: \"*Bad plot*\"",
    "# `comment_with_code` is not a rendered heading",
    "plot <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +",
    "  ggplot2::geom_point() +",
    "  ggplot2::labs(title = \"Baked title\")",
    "plot",
    "```",
    "",
    "```{r bad_model}",
    "x <- 1",
    "```",
    "",
    "```{r bad_table}",
    "knitr::kable(mtcars[1:2, 1:2])",
    "```"
  ), qmd)

  checks <- validate_report_style(qmd)

  expect_true(any(checks$check == "html house style enabled" & !checks$passed))
  expect_true(any(checks$check == "no inline code in headings" & !checks$passed))
  expect_true(any(checks$check == "no kable output" & !checks$passed))
  expect_true(any(checks$check == "no baked ggplot figure titles or notes" & !checks$passed))
  expect_true(any(checks$check == "figure captions placed above figures" & !checks$passed))
  expect_true(any(checks$check == "figure notes in adjacent asides" & !checks$passed))
  expect_true(any(checks$check == "no hard-coded figure numbers in prose" & !checks$passed))
  expect_true(any(checks$check == "model chunks cached" & !checks$passed))
  expect_true(any(checks$check == "display chunks declare dependencies" & !checks$passed))
})

test_that("validate_report_style passes a minimal good figure", {
  qmd <- tempfile(fileext = ".qmd")
  writeLines(c(
    "---",
    "title: Good Example",
    "format:",
    "  html:",
    "    toc: true",
    "    toc-title: \"Contents\"",
    "    toc-depth: 3",
    "    toc-location: left",
    "    number-sections: true",
    "    number-depth: 3",
    "    theme:",
    "      light: cosmo",
    "      dark: darkly",
    "    respect-user-color-scheme: true",
    "    embed-resources: true",
    "    smooth-scroll: true",
    "    anchor-sections: true",
    "    df-print: paged",
    "    link-external-icon: true",
    "    css: report-style.css",
    "---",
    "",
    "```{r data_model}",
    "#| cache: true",
    "# `comment_with_code` is not a rendered heading",
    "x <- 1",
    "```",
    "",
    "```{r fig-good}",
    "#| fig-cap: \"*Good plot*\"",
    "#| fig-cap-location: top",
    "#| dependson: data_model",
    "ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +",
    "  ggplot2::geom_point() +",
    "  ggplot2::labs(x = \"Miles per gallon\", y = \"Weight\")",
    "reader_table <- function(data, title) data",
    "plot(1, 1)",
    "```",
    "",
    "::: {.aside}",
    "_Note._ A good note.",
    ":::"
  ), qmd)

  checks <- validate_report_style(qmd)

  expect_true(all(checks$passed[checks$check != "rendered html is not stale"]))
})
