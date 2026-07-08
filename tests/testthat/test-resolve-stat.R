test_that("resolve_stat() returns single-statistic templates", {
  result <- resolve_stat("single", NULL, NULL)
  expect_equal(result, list(cat = "{p}%", cont = "{mean}"))
})

test_that("resolve_stat() returns both-statistic templates", {
  result <- resolve_stat("both", NULL, NULL)
  expect_equal(result, list(cat = "{p}% ({n})", cont = "{mean} ({sd})"))
})

test_that("resolve_stat() returns custom templates", {
  result <- resolve_stat("custom", "{n}", "{median}")
  expect_equal(result, list(cat = "{n}", cont = "{median}"))
})

test_that("resolve_stat() errors on an invalid stat_option", {
  expect_snapshot(error = TRUE, resolve_stat("nope", NULL, NULL))
})
