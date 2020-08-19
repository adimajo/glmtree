context("test-glmtree")

test_that("glmtree throws right errors", {
  data <- generateData(n = 100, scenario = "no tree")
  expect_error(glmtree(x = data[c(1:99), c("x1", "x2")], y = data$y, K = 5, iterations = 80, criterion = "aic"))
  data$x1 <- as.integer(data$x1)
  expect_error(glmtree(x = data[, c("x1", "x2")], y = data$y, K = 5, iterations = 80, criterion = "aic"))
  data <- generateData(n = 100, scenario = "no tree")
  expect_error(glmtree(x = data[, c("x1", "x2")], y = data$y, K = 5, iterations = 80, criterion = "toto"))
  data$y[data$y == 0] <- rbinom(sum(data$y == 0), 1, 0.5) * 2
  expect_error(glmtree(x = data[, c("x1", "x2")], y = data$y, K = 5, iterations = 80, criterion = "aic"))
  data <- generateData(n = 100, scenario = "no tree")
  # expect_warning(glmtree(x = data[, c("x1", "x2")], y = data$y, K = 5, iterations = 80, criterion = "gini", validation = FALSE),
  #                "Using Gini index on training set might yield an overfitted model.",
  #                fixed = TRUE)
  expect_warning(glmtree(x = data[, c("x1", "x2")], y = data$y, K = 5, iterations = 80, criterion = "aic", validation = TRUE),
    "No need to penalize the log-likelihood when a validation set is used. Using log-likelihood instead of AIC/BIC.",
    fixed = TRUE
  )
})
