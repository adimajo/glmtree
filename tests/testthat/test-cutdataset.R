test_that("cutDataset works with only validation", {
  set.seed(1)
  split <- cutDataset(20, c(0.1), test = FALSE, validation = TRUE)
  expect_length(split$validation, 2)
  expect_length(split$train, 18)
  expect_null(split$test)
})

test_that("cutDataset works with only test", {
  set.seed(1)
  split <- cutDataset(20, c(0.25), test = TRUE, validation = FALSE)
  expect_length(split$test, 5)
  expect_length(split$train, 15)
  expect_null(split$validation)
})

test_that("cutDataset works with test and validation, and sets are disjoint and complete", {
  set.seed(1)
  split <- cutDataset(20, c(0.1, 0.3), test = TRUE, validation = TRUE)
  expect_length(split$test, 2)
  expect_length(split$validation, 6)
  expect_length(split$train, 12)
  expect_setequal(c(split$train, split$test, split$validation), 1:20)
})

test_that("cutDataset works with no test and no validation", {
  split <- cutDataset(20, c(0.1, 0.1), test = FALSE, validation = FALSE)
  expect_setequal(split$train, 1:20)
  expect_null(split$validation)
  expect_null(split$test)
  expect_setequal(cutDataset(20, test = FALSE, validation = FALSE)$train, 1:20)
})

test_that("cutDataset errors with invalid proportions", {
  one <- "Argument proportions should contain 1 argument strictly between 0 and 1"
  expect_error(cutDataset(20, c(-0.1), test = TRUE, validation = FALSE), one, fixed = TRUE)
  expect_error(cutDataset(20, c(1.1), test = TRUE, validation = FALSE), one, fixed = TRUE)
  expect_error(cutDataset(20, c(-0.1), test = FALSE, validation = TRUE), one, fixed = TRUE)
  expect_error(cutDataset(20, c(1.1), test = FALSE, validation = TRUE), one, fixed = TRUE)
  two <- "Argument proportions should contain 2 positive arguments which sum should be less than 1"
  expect_error(cutDataset(20, c(0.1), test = TRUE, validation = TRUE), two, fixed = TRUE)
  expect_error(cutDataset(20, c(0.5, 0.5), test = TRUE, validation = TRUE), two, fixed = TRUE)
})
