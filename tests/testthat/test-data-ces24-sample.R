test_that("CES24_sample loads with the expected columns", {
  expect_s3_class(CES24_sample, "data.frame")
  expect_contains(
    names(CES24_sample),
    c(
      "commonweight",
      "gender4",
      "educ",
      "race",
      "CC24_301",
      "CC24_302",
      "CC24_303"
    )
  )
  expect_gt(nrow(CES24_sample), 0)
})

test_that("crosstab() runs unweighted on a subset of CES24_sample", {
  data <- CES24_sample[1:200, ]

  tbl <- suppressMessages(crosstab(
    data,
    c("CC24_301", "CC24_302"),
    c("gender4", "educ")
  ))

  expect_s3_class(tbl, "gtsummary")
})

test_that("crosstab() runs weighted on a subset of CES24_sample", {
  data <- CES24_sample[1:200, ]

  tbl <- suppressMessages(
    crosstab(
      data,
      c("CC24_301", "CC24_302"),
      c("gender4", "educ"),
      weight = "commonweight"
    )
  )

  expect_s3_class(tbl, "gtsummary")
})
