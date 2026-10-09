small_controls <- partykit::ctree_control(alpha = 0.1, minbucket = 40, maxdepth = 3)
features <- c("x1", "x2", "x3")

fit_small <- function(data, ..., iterations = 15) {
  suppressWarnings(glmtree(
    x = data[, features], y = data$y, K = 4, iterations = iterations,
    ctree_controls = small_controls, ...
  ))
}

test_that("glmtree throws right errors", {
  set.seed(1)
  data <- generateData(n = 100, scenario = "no tree")
  expect_error(glmtree(x = data[c(1:99), c("x1", "x2")], y = data$y, K = 5, iterations = 80, criterion = "aic"), "same length")
  expect_error(glmtree(x = data[, c("x1", "x2")], y = data$y, K = 5, iterations = 80, criterion = "toto"), "not supported")
  data$x3 <- as.Date("2020-01-01") + seq_len(100)
  expect_error(glmtree(x = data[, c("x1", "x3")], y = data$y), "Unsupported data types")
  data$x3 <- replace(data$x1, 3, NA)
  expect_error(glmtree(x = data[, c("x1", "x3")], y = data$y), "missing")
  expect_error(glmtree(x = data[, c("x1", "x2")], y = replace(data$y, 1:3, 2)), "two classes")
  expect_error(glmtree(x = data[, c("x1", "x2")], y = data$y, K = 1), "K must")
  expect_error(glmtree(x = data[, c("x1", "x2")], y = data$y, iterations = 1), "iterations must")
  expect_error(glmtree(x = data[, c("x1", "x2")], y = data$y, iterations = 10, burn_in = 10), "burn_in")
  expect_error(glmtree(x = data[, c("x1", "x2")], y = data$y, patience = 0), "patience")
})

test_that("glmtree warns about criteria used on the wrong data", {
  set.seed(1)
  data <- generateData(n = 100, scenario = "no tree")
  expect_warning(
    glmtree(x = data[, c("x1", "x2")], y = data$y, K = 3, iterations = 3, criterion = "gini", ctree_controls = small_controls),
    "Using Gini index on training set might yield an overfitted model.",
    fixed = TRUE
  )
  expect_warning(
    glmtree(
      x = data[, c("x1", "x2")], y = data$y, K = 3, iterations = 3, criterion = "aic", validation = TRUE,
      ctree_controls = small_controls
    ),
    "No need to penalize the log-likelihood when a validation set is used. Using log-likelihood instead of AIC/BIC.",
    fixed = TRUE
  )
})

test_that("glmtree finds the segments of a true tree and beats a single logistic regression", {
  set.seed(2)
  data <- generateData(n = 2400, scenario = "tree")
  train <- sample(2400, 1600)
  fit <- suppressWarnings(glmtree(
    x = data[train, features], y = data$y[train], K = 5, iterations = 60,
    ctree_controls = partykit::ctree_control(alpha = 0.1, minbucket = 80, maxdepth = 4)
  ))
  expect_s4_class(fit, "glmtree")
  pred <- predict(fit, data[-train, features])
  logit <- stats::glm(y ~ x1 + x2 + x3, data = data[train, ], family = stats::binomial())
  gini_tree <- normalizedGini(data$y[-train], pred)
  gini_logit <- normalizedGini(data$y[-train], stats::predict(logit, data[-train, ], type = "response"))
  expect_gt(gini_tree, gini_logit + 0.1)
})

test_that("short chains work and the best iteration is after the burn-in", {
  set.seed(3)
  data <- generateData(n = 300, scenario = "tree")
  fit <- fit_small(data, iterations = 2)
  expect_equal(fit@performance$best_iteration, 2)
  fit <- fit_small(data, iterations = 20, burn_in = 5)
  expect_gt(fit@performance$best_iteration, 5)
  expect_length(fit@performance$criterionEvolution, 20)
  expect_equal(fit@performance$performance, fit@performance$criterionEvolution[[fit@performance$best_iteration]])
})

test_that("test and validation sets are held out from the fit and used where documented", {
  set.seed(4)
  data <- generateData(n = 600, scenario = "tree")
  fit <- fit_small(data, test = TRUE, validation = TRUE, proportions = c(0.2, 0.25), criterion = "gini")
  expect_equal(unname(fit@parameters$sizes), c(330, 150, 120))
  # the criterion at each iteration is on the validation set; the performance is on the test set
  evolution <- unlist(fit@performance$criterionEvolution)
  expect_true(is.na(evolution[1]))
  expect_false(anyNA(evolution[-1]))
  expect_false(isTRUE(all.equal(fit@performance$performance, evolution[fit@performance$best_iteration])))

  fit <- fit_small(data, validation = TRUE, proportions = 0.3, criterion = "aic")
  expect_equal(unname(fit@parameters$sizes), c(420, 180, 0))
  expect_lt(max(unlist(fit@performance$criterionEvolution), na.rm = TRUE), 0)

  fit <- fit_small(data, test = TRUE, proportions = 0.5, criterion = "bic")
  expect_equal(unname(fit@parameters$sizes), c(300, 0, 300))
})

test_that("patience stops the chain", {
  set.seed(5)
  data <- generateData(n = 400, scenario = "no tree")
  fit <- suppressWarnings(glmtree(
    x = data[, c("x1", "x2")], y = data$y, K = 3, iterations = 100, burn_in = 2, patience = 3,
    ctree_controls = small_controls
  ))
  done <- length(fit@performance$criterionEvolution)
  expect_lt(done, 100)
  expect_gte(done - fit@performance$best_iteration, 3)
})

test_that("results are reproducible and verbose prints the criterion", {
  data <- generateData(n = 300, scenario = "tree")
  set.seed(6)
  a <- fit_small(data)
  set.seed(6)
  b <- fit_small(data)
  expect_equal(unlist(a@performance$criterionEvolution), unlist(b@performance$criterionEvolution))
  expect_silent(fit_small(data))
  expect_message(fit_small(data, verbose = TRUE, iterations = 3), "criterion for iteration 1")
})

test_that("inputs: integers, matrices, characters, logicals and factor / logical responses", {
  set.seed(7)
  data <- generateData(n = 300, scenario = "tree")
  x <- data.frame(
    i = as.integer(round(data$x2 * 3)), ch = as.character(data$x1), lg = data$x3 > 0
  )
  fit <- suppressWarnings(glmtree(x, factor(data$y, labels = c("no", "yes")), K = 3, iterations = 5, ctree_controls = small_controls))
  expect_length(predict(fit, x), 300)
  fit <- suppressWarnings(glmtree(as.matrix(data[, c("x2", "x3")]), data$y == 1, K = 3, iterations = 5, ctree_controls = small_controls))
  expect_length(predict(fit, as.matrix(data[, c("x2", "x3")])), 300)
  # the larger value / second level is the positive class
  y_flipped <- 1 - data$y
  fit1 <- fit_small(data, iterations = 3)
  expect_true(all(predict(fit1, data[, features]) >= 0))
  expect_equal(encode_response(y_flipped), 1 - encode_response(data$y))
})

test_that("a collapsing chain still returns a model", {
  set.seed(8)
  data <- generateData(n = 60, scenario = "no tree")
  # minbucket larger than the data: the tree cannot split, only one segment remains
  fit <- suppressWarnings(glmtree(
    x = data[, c("x1", "x2")], y = data$y, K = 2, iterations = 6,
    ctree_controls = partykit::ctree_control(minbucket = 200)
  ))
  expect_length(predict(fit, data[, c("x1", "x2")]), 60)
})

test_that("show describes the fit", {
  set.seed(9)
  data <- generateData(n = 300, scenario = "tree")
  fit <- fit_small(data)
  expect_output(show(fit), "Logistic regression tree fitted by SEM")
  expect_output(print(fit), "bic of the best tree")
})
