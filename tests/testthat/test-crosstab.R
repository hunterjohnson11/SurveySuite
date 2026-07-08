# Unweighted -----------------------------------------------------------------

test_that("crosstab() returns a gtsummary object with the expected spanners", {
  data <- make_crosstab_data()

  tbl <- suppressMessages(crosstab(data, c("age"), c("group")))

  expect_s3_class(tbl, "gtsummary")
  spanners <- unique(tbl$table_styling$spanning_header$spanning_header)
  expect_setequal(spanners, c("**Total**", "**group**"))
})

test_that("crosstab() stat_option = 'single' shows percentage only", {
  data <- make_crosstab_data()

  tbl <- suppressMessages(crosstab(
    data,
    c("age"),
    c("group"),
    stat_option = "single"
  ))

  age_row <- tbl$table_body[
    tbl$table_body$variable == "age" & tbl$table_body$row_type == "level",
  ]
  expect_true(all(grepl("^[0-9]+%$", age_row$stat_0_1)))
})

test_that("crosstab() stat_option = 'both' shows percentage and n", {
  data <- make_crosstab_data()

  tbl <- suppressMessages(crosstab(
    data,
    c("age"),
    c("group"),
    stat_option = "both"
  ))

  age_row <- tbl$table_body[
    tbl$table_body$variable == "age" & tbl$table_body$row_type == "level",
  ]
  expect_true(all(grepl("^[0-9]+% \\([0-9]+\\)$", age_row$stat_0_1)))
})

test_that("crosstab() stat_option = 'custom' uses the supplied statistic templates", {
  data <- make_crosstab_data()

  tbl <- suppressMessages(crosstab(
    data,
    c("age"),
    c("group"),
    stat_option = "custom",
    custom_stat_cat = "{n}",
    custom_stat_cont = "{median}"
  ))

  age_row <- tbl$table_body[
    tbl$table_body$variable == "age" & tbl$table_body$row_type == "level",
  ]
  expect_true(all(grepl("^[0-9]+$", age_row$stat_0_1)))
})

test_that("crosstab() errors on an invalid stat_option", {
  data <- make_crosstab_data()

  expect_snapshot(
    error = TRUE,
    suppressMessages(crosstab(data, c("age"), c("group"), stat_option = "nope"))
  )
})

test_that("crosstab() collapses multiselect groups in the final table body", {
  data <- make_crosstab_data()
  main_vars <- c("M_TOPIC_r1", "M_TOPIC_r2", "M_TOPIC_r3")

  tbl <- suppressMessages(crosstab(data, main_vars, c("group")))

  m_rows <- tbl$table_body[tbl$table_body$variable %in% main_vars, ]
  expect_equal(sum(m_rows$row_type == "label"), 1)
  expect_false(any(grepl("^NO TO", m_rows$label)))
  expect_false(any(m_rows$row_type == "missing"))
})

# Weighted ---------------------------------------------------------------

test_that("crosstab() builds a weighted design when weight is supplied", {
  data <- make_crosstab_data()

  tbl <- suppressMessages(crosstab(
    data,
    c("age"),
    c("group"),
    weight = "weight"
  ))

  expect_s3_class(tbl, "gtsummary")
  spanners <- unique(tbl$table_styling$spanning_header$spanning_header)
  expect_setequal(spanners, c("**Total**", "**group**"))
})

test_that("crosstab() weighted path also collapses multiselect groups", {
  data <- make_crosstab_data()
  main_vars <- c("M_TOPIC_r1", "M_TOPIC_r2", "M_TOPIC_r3")

  tbl <- suppressMessages(crosstab(
    data,
    main_vars,
    c("group"),
    weight = "weight"
  ))

  m_rows <- tbl$table_body[tbl$table_body$variable %in% main_vars, ]
  expect_equal(sum(m_rows$row_type == "label"), 1)
  expect_false(any(grepl("^NO TO", m_rows$label)))
})

test_that("crosstab() accepts a strata column alongside weight without erroring", {
  data <- make_crosstab_data()
  data$strata_col <- rep(c(1, 2), length.out = nrow(data))

  tbl <- suppressMessages(
    crosstab(
      data,
      c("age"),
      c("group"),
      weight = "weight",
      strata = "strata_col"
    )
  )

  expect_s3_class(tbl, "gtsummary")
})
