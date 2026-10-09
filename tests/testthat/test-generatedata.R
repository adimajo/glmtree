test_that("generateData errors with invalid input", {
  expect_error(generateData(n = 9))
  expect_error(generateData(n = 10, visualize = "toto"))
  expect_error(generateData(n = 10, scenario = "toto"))
})

test_that("generateData in tree scenario", {
  set.seed(1)
  tree_data <- generateData(scenario = "tree")
  expect_equal(colnames(tree_data), c("x1", "x2", "x3", "y", "c"))
  expect_equal(class(tree_data$x1), "factor")
  expect_equal(class(tree_data$x2), "numeric")
  expect_equal(class(tree_data$x3), "numeric")
  expect_equal(class(tree_data$y), "numeric")
  expect_equal(class(tree_data$c), "numeric")
  expect_equal(nrow(tree_data), 100)
  expect_equal(as.vector(table(tree_data$c)), c(33, 33, 34))
})

test_that("generateData in tree scenario draws from the documented mechanism", {
  set.seed(2)
  data <- generateData(n = 30000, scenario = "tree")
  # cluster k uses levels 2k - 1 and 2k of x1
  x1 <- as.integer(as.character(data$x1))
  expect_true(all(x1 >= 2 * data$c - 1 & x1 <= 2 * data$c))
  expect_equal(sd(data$x2), 1.5, tolerance = 0.03)
  # logistic regressions of the clusters recover the true coefficients
  theta <- rbind(c(0, -1, 0.5), c(0, -0.5, 1.5), c(0, 1, -0.5))
  for (k in 1:3) {
    fit <- stats::glm(y ~ x2 + x3, data = data[data$c == k, ], family = stats::binomial())
    expect_equal(unname(stats::coef(fit)), theta[k, ], tolerance = 0.12)
  }
})

test_that("generateData in tree scenario - plot", {
  set.seed(1)
  expect_no_error(generateData(scenario = "tree", visualize = TRUE))
})

test_that("generateData in no tree scenario", {
  set.seed(3)
  data <- generateData(n = 30000, scenario = "no tree")
  expect_equal(colnames(data), c("x1", "x2", "y"))
  expect_equal(class(data$x1), "numeric")
  expect_equal(class(data$y), "numeric")
  expect_equal(nrow(data), 30000)
  fit <- stats::glm(y ~ x1 + x2, data = data, family = stats::binomial())
  expect_equal(unname(stats::coef(fit)), c(3, 0.5, -1), tolerance = 0.12)
  expect_no_error(generateData(scenario = "no tree", visualize = TRUE))
})
