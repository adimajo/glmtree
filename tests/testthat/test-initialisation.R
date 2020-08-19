context("test-initialisation")

test_that("generateData errors with invalid input", {
  init = initialization(K = 3, n = 9)
  expect_length(init, 9)
  expect_equal(class(init), "factor")
  expect_equal(levels(init), c("1", "2", "3"))
})
