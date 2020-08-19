context("test-initialization")

test_that("initialization works", {
  init <- initialization(10, 100)
  expect_length(init, 100)
  expect_vector(init)
  expect_true(is.factor(init))
  expect_equal(levels(init), levels(factor(1:10)))
})
