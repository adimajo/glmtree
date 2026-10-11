test_that("initialization works", {
  set.seed(1)
  init <- initialization(10, 100)
  expect_length(init, 100)
  expect_type(init, "integer")
  expect_true(all(init %in% 1:10))
})
