context("test-generatedata")

test_that("generateData errors with invalid input", {
  expect_error(generateData(n = 9))
  expect_error(generateData(n = 10, visualize = "toto"))
  expect_error(generateData(n = 10, scenario = "toto"))
})

test_that("generateData in tree scenario", {
  tree_data <- generateData(scenario = "tree")
  expect_equal(colnames(tree_data), c("x1", "x2", "x3", "y", "c"))
  expect_equal(class(tree_data$x1), "factor")
  expect_equal(class(tree_data$x2), "numeric")
  expect_equal(class(tree_data$x3), "numeric")
  expect_equal(class(tree_data$y), "numeric")
  expect_equal(class(tree_data$c), "numeric")
  expect_length(tree_data, 5)
  expect_length(tree_data[, 1], 100)
})

test_that("generateData in tree scenario - plot", {
  tree_data <- generateData(
    scenario = "tree",
    visualize = TRUE
  )
})

test_that("generateData in no tree scenario", {
  tree_data <- generateData(scenario = "no tree")
  expect_equal(colnames(tree_data), c("x1", "x2", "y"))
  expect_equal(class(tree_data$x1), "numeric")
  expect_equal(class(tree_data$x2), "numeric")
  expect_equal(class(tree_data$y), "numeric")
  expect_length(tree_data, 3)
  expect_length(tree_data[, 1], 100)
})
