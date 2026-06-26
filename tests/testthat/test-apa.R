test_that("APA number helpers format common report values", {
  expect_equal(apa_number(0.125, digits = 2), "0.12")
  expect_equal(apa_number(0.125, digits = 2, omit_leading_zero = TRUE), ".12")
  expect_equal(apa_table_number(c(1, 1.25), digits = 2), c("1", "1.25"))
  expect_equal(apa_p_prose(c(0.0005, 0.02)), c("p < .001", "p = .020"))
  expect_equal(apa_percent(0.125), "12.5%")
  expect_equal(apa_ci(0.1, 0.2, omit_leading_zero = TRUE), "[.10, .20]")
})

test_that("reader_table returns a gt table", {
  table <- reader_table(
    tibble::tibble(value = 1.25, p_value = 0.02),
    label = "Table 1",
    title = "Example table",
    note = "Example note."
  )
  expect_s3_class(table, "gt_tbl")
})

test_that("reader_table can defer numbering and captions to Quarto", {
  table <- reader_table(
    tibble::tibble(value = 1.25, p_value = 0.02),
    note = "Example note."
  )
  expect_s3_class(table, "gt_tbl")
})
