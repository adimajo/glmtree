context("test-normalizedGini")

test_that("normalizedGini works", {
  actual <- c(1, 1, 0, 0, 0)
  predicted <- c(0.8, 0.4, 0.5, 0.3, 0.2)
  gini <- normalizedGini(actual = actual, predicted = predicted)
  expect_lt(gini, 1)
  expect_gt(gini, 0)
  expect_error(normalizedGini(c(1, 1, 1, 0, 0), c(0.7, 0.9, 0.5, 0.6)))
})
