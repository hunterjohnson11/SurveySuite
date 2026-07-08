test_that("detect_multiselect_groups() returns an empty list when no M_ vars are present", {
  result <- detect_multiselect_groups(c("age", "gender", "income"))
  expect_equal(result, list())
})

test_that("detect_multiselect_groups() groups by shared prefix with default stem", {
  vars <- c("M_TOPIC_r1", "M_TOPIC_r2", "M_OTHER_r1", "age")
  result <- detect_multiselect_groups(vars)

  expect_setequal(names(result), c("M_TOPIC_", "M_OTHER_"))
  expect_setequal(result[["M_TOPIC_"]], c("M_TOPIC_r1", "M_TOPIC_r2"))
  expect_setequal(result[["M_OTHER_"]], "M_OTHER_r1")
})

test_that("detect_multiselect_groups() respects a custom stem", {
  vars <- c("M_TOPICq1", "M_TOPICq2", "M_OTHERq1")
  result <- detect_multiselect_groups(vars, stem = "q")

  expect_setequal(names(result), c("M_TOPIC", "M_OTHER"))
  expect_setequal(result[["M_TOPIC"]], c("M_TOPICq1", "M_TOPICq2"))
})

test_that("detect_multiselect_groups() excludes non-M_ variables from groups", {
  vars <- c("M_TOPIC_r1", "age", "gender")
  result <- detect_multiselect_groups(vars)

  expect_equal(unlist(result, use.names = FALSE), "M_TOPIC_r1")
})
