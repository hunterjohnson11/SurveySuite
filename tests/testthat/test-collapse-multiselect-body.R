test_that("collapse_multiselect_body() returns the body unchanged when there are no M_ vars", {
  body <- make_table_body(list(
    list(
      variable = "age",
      row_type = "label",
      label = "age",
      stat_0_1 = NA_character_
    ),
    list(
      variable = "gender",
      row_type = "label",
      label = "gender",
      stat_0_1 = NA_character_
    )
  ))

  result <- collapse_multiselect_body(body, c("age", "gender"))

  expect_equal(result, body)
})

test_that("collapse_multiselect_body() collapses a single multiselect group to one label + sorted levels", {
  body <- make_table_body(list(
    list(
      variable = "M_TOPIC_r1",
      row_type = "label",
      label = "M_TOPIC_r1",
      stat_0_1 = NA_character_
    ),
    list(
      variable = "M_TOPIC_r1",
      row_type = "level",
      label = "Selected",
      stat_0_1 = "40% (4)"
    ),
    list(
      variable = "M_TOPIC_r1",
      row_type = "level",
      label = "NO TO Topic 1",
      stat_0_1 = "60% (6)"
    ),
    list(
      variable = "M_TOPIC_r2",
      row_type = "label",
      label = "M_TOPIC_r2",
      stat_0_1 = NA_character_
    ),
    list(
      variable = "M_TOPIC_r2",
      row_type = "level",
      label = "Selected",
      stat_0_1 = "70% (7)"
    ),
    list(
      variable = "M_TOPIC_r2",
      row_type = "level",
      label = "NO TO Topic 2",
      stat_0_1 = "30% (3)"
    ),
    list(
      variable = "M_TOPIC_r2",
      row_type = "missing",
      label = "Unknown",
      stat_0_1 = "5% (1)"
    )
  ))

  result <- collapse_multiselect_body(body, c("M_TOPIC_r1", "M_TOPIC_r2"))

  expect_equal(nrow(result), 3)
  expect_equal(result$row_type, c("label", "level", "level"))
  # "NO TO ..." and "missing" rows dropped; remaining levels sorted desc by pct
  expect_equal(result$label, c("M_TOPIC_r1", "Selected", "Selected"))
  expect_equal(result$stat_0_1, c(NA, "70% (7)", "40% (4)"))
})

test_that("collapse_multiselect_body() keeps non-multiselect rows around the collapsed block", {
  body <- make_table_body(list(
    list(
      variable = "age",
      row_type = "label",
      label = "age",
      stat_0_1 = NA_character_
    ),
    list(
      variable = "M_TOPIC_r1",
      row_type = "label",
      label = "M_TOPIC_r1",
      stat_0_1 = NA_character_
    ),
    list(
      variable = "M_TOPIC_r1",
      row_type = "level",
      label = "Selected",
      stat_0_1 = "40% (4)"
    ),
    list(
      variable = "M_TOPIC_r1",
      row_type = "level",
      label = "NO TO Topic 1",
      stat_0_1 = "60% (6)"
    ),
    list(
      variable = "gender",
      row_type = "label",
      label = "gender",
      stat_0_1 = NA_character_
    )
  ))

  result <- collapse_multiselect_body(body, c("age", "M_TOPIC_r1", "gender"))

  expect_equal(result$variable, c("age", "M_TOPIC_r1", "M_TOPIC_r1", "gender"))
  expect_equal(result$row_type, c("label", "label", "level", "label"))
})

test_that("collapse_multiselect_body() sorts groups alphabetically by prefix, independent of first-appearance order", {
  body <- make_table_body(list(
    list(
      variable = "age",
      row_type = "label",
      label = "age",
      stat_0_1 = NA_character_
    ),
    list(
      variable = "M_TOPIC_r1",
      row_type = "label",
      label = "M_TOPIC_r1",
      stat_0_1 = NA_character_
    ),
    list(
      variable = "M_TOPIC_r1",
      row_type = "level",
      label = "Selected",
      stat_0_1 = "40% (4)"
    ),
    list(
      variable = "M_OTHER_r1",
      row_type = "label",
      label = "M_OTHER_r1",
      stat_0_1 = NA_character_
    ),
    list(
      variable = "M_OTHER_r1",
      row_type = "level",
      label = "Selected",
      stat_0_1 = "20% (2)"
    ),
    list(
      variable = "gender",
      row_type = "label",
      label = "gender",
      stat_0_1 = NA_character_
    )
  ))

  result <- collapse_multiselect_body(
    body,
    c("age", "M_TOPIC_r1", "M_OTHER_r1", "gender")
  )

  expect_equal(
    result$variable,
    c("age", "M_OTHER_r1", "M_OTHER_r1", "M_TOPIC_r1", "M_TOPIC_r1", "gender")
  )
})
