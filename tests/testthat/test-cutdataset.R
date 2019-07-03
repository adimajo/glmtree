context("test-cutdataset")

test_that("cutDataset works with only validation", {
  test_de_cut = cutDataset(20, c(0.1), test = F, validation = T)
  expect_type(test_de_cut[[1]], "integer")
  expect_type(test_de_cut[[2]], "integer")
  expect_null(test_de_cut[[3]])
})

test_that("cutDataset works with only test", {
  test_de_cut = cutDataset(20, c(0.1), test = T, validation = F)
  expect_type(test_de_cut[[1]], "integer")
  expect_type(test_de_cut[[2]], "integer")
  expect_null(test_de_cut[[3]])
})

test_that("cutDataset errors with test and validation and only one proportion", {
  expect_error(cutDataset(20, c(0.1), test = T, validation = T))
})

test_that("cutDataset works with test and validation", {
  test_de_cut = cutDataset(20, c(0.1, 0.1), test = T, validation = T)
  expect_type(test_de_cut[[1]], "integer")
  expect_type(test_de_cut[[2]], "integer")
  expect_type(test_de_cut[[3]], "integer")
})

test_that("cutDataset works with no test and no validation", {
  test_de_cut1 = cutDataset(20, c(0.1, 0.1), test = F, validation = F)
  test_de_cut2 = cutDataset(20, c(0.1), test = F, validation = F)
  test_de_cut3 = cutDataset(20, test = F, validation = F)
  expect_type(test_de_cut1[[1]], "integer")
  expect_type(test_de_cut2[[1]], "integer")
  expect_type(test_de_cut3[[1]], "integer")
  expect_null(test_de_cut1[[2]])
  expect_null(test_de_cut2[[2]])
  expect_null(test_de_cut3[[2]])
  expect_null(test_de_cut1[[3]])
  expect_null(test_de_cut2[[3]])
  expect_null(test_de_cut3[[3]])
})
