test_that("draw_segments samples proportionally to the weights and never draws a null weight", {
  set.seed(1)
  weights <- matrix(rep(c(0.2, 0.8, 0), each = 20000), ncol = 3)
  draws <- draw_segments(weights, c(10L, 20L, 30L))
  expect_equal(mean(draws == 20L), 0.8, tolerance = 0.02)
  expect_false(any(draws == 30L))
})

test_that("training_criterion penalizes each segment by the log of its size for the BIC", {
  y <- c(1, 0, 1, 1, 0, 0, 1, 0)
  p <- c(0.7, 0.3, 0.6, 0.8, 0.4, 0.2, 0.9, 0.1)
  seg <- c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L)
  loglik <- tapply(y * log(p) + (1 - y) * log(1 - p), seg, sum)
  expect_equal(training_criterion("bic", y, p, seg, c(2, 3)), -sum(-2 * loglik + log(c(4, 4)) * c(2, 3)))
  expect_equal(training_criterion("aic", y, p, seg, c(2, 3)), -sum(-2 * loglik + 2 * c(2, 3)))
  expect_equal(training_criterion("gini", y, p, seg, c(2, 3)), normalizedGini(y, p))
  expect_equal(holdout_criterion("aic", y, p), sum(loglik))
  expect_equal(holdout_criterion("gini", y, p), normalizedGini(y, p))
})

test_that("fit_logit sets aliased coefficients to zero and fit_segments is one regression per label", {
  set.seed(1)
  X <- cbind(`(Intercept)` = 1, a = rnorm(50))
  X <- cbind(X, b = 2 * X[, "a"])
  y <- rbinom(50, 1, 0.5)
  fit <- fit_logit(X, y)
  expect_equal(unname(fit$coefficients["b"]), 0)
  expect_equal(fit$rank, 2)
  seg <- rep(1:2, 25)
  fits <- fit_segments(X, y, seg, 1:2)
  expect_equal(unname(fits$B[, 2]), unname(fit_logit(X[seg == 2, ], y[seg == 2])$coefficients))
})

test_that("segment_probabilities uses the fallback for segments without regression", {
  X <- cbind(1, c(0, 1, 2))
  B <- cbind(c(0, 1))
  p <- segment_probabilities(X, c(5L, 5L, 9L), 5L, B, fallback = c(10, 0))
  expect_equal(p, stats::plogis(c(0, 1, 10)))
})

test_that("check_features and align_features", {
  x <- check_features(data.frame(i = 1:4, ch = c("a", "b", "a", "b"), lg = c(TRUE, FALSE, TRUE, TRUE)))
  expect_true(is.numeric(x$i))
  expect_true(is.factor(x$ch) && is.factor(x$lg))
  features <- feature_description(x)
  aligned <- align_features(data.frame(i = 1:2, ch = c("a", "z"), lg = c(TRUE, TRUE)), features)
  expect_equal(aligned$unseen, c(FALSE, TRUE))
  expect_true(is.na(aligned$x$ch[2]))
  X <- design_matrix(aligned$x, features)
  expect_equal(colnames(X), c("(Intercept)", "i", "chb", "lgTRUE"))
  expect_equal(unname(X[2, "chb"]), 0)
  # a constant factor carries no information
  constant <- feature_description(check_features(data.frame(a = c(1, 2), f = c("u", "u"))))
  expect_equal(colnames(design_matrix(check_features(data.frame(a = c(1, 2), f = c("u", "u"))), constant)), c("(Intercept)", "a"))
  expect_error(check_features(data.frame(a = c(1, Inf))), "missing or infinite")
})

test_that("encode_response", {
  expect_equal(encode_response(c("b", "a", "b")), c(1, 0, 1))
  expect_equal(encode_response(c(TRUE, FALSE)), c(1, 0))
  expect_equal(encode_response(factor(c("n", "y", "n"), levels = c("y", "n"))), c(1, 0, 1))
  expect_error(encode_response(c(1, 2, 3)), "two classes")
  expect_error(encode_response(c(1, NA, 0)), "missing")
})
