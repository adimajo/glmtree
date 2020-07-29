context("test-generatedata")

test_that("generateData yields correct errors", {
  expect_error(generateData(n=9))
  expect_error(generateData(visualize = "toto"))
  expect_error(generateData(scenario = "toto"))
})

test_that("generateData works for tree", {
  tree = generateData(scenario = "tree")
  expect_length(tree, 5)
  expect_length(tree[, 1], 100)
})

test_that("generateData works for tree", {
  no_tree = generateData(scenario = "no tree")
  expect_length(no_tree, 3)
  expect_length(no_tree[, 1], 100)
})
