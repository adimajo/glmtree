small_controls <- partykit::ctree_control(alpha = 0.1, minbucket = 40, maxdepth = 3)

test_that("predict returns one probability or segment per row and matches the stored regressions", {
  set.seed(1)
  data <- generateData(n = 600, scenario = "tree")
  fit <- suppressWarnings(glmtree(data[, c("x1", "x2", "x3")], data$y, K = 4, iterations = 20, ctree_controls = small_controls))
  pred <- predict(fit, data[, c("x1", "x2", "x3")])
  expect_length(pred, 600)
  expect_true(all(pred >= 0 & pred <= 1))
  segment <- predict(fit, data[, c("x1", "x2", "x3")], type = "segment")
  expect_true(all(as.character(segment) %in% names(fit@best.tree$glms)))
  # same coefficients in the glm objects and in the prediction matrix
  for (label in names(fit@best.tree$glms)) {
    coefficients <- stats::coef(fit@best.tree$glms[[label]])
    coefficients[is.na(coefficients)] <- 0
    expect_equal(unname(coefficients), unname(fit@best.tree$coefficients[, label]), tolerance = 1e-6)
  }
  # recompute one probability by hand
  row <- 7
  X <- design_matrix(align_features(data[row, c("x1", "x2", "x3")], fit@best.tree$features)$x, fit@best.tree$features)
  expect_equal(pred[row], unname(stats::plogis(sum(X * fit@best.tree$coefficients[, as.character(segment[row])]))))
  # order does not matter and a single row works
  expect_equal(predict(fit, data[rev(seq_len(600)), c("x1", "x2", "x3")]), rev(pred))
  expect_length(predict(fit, data[1, c("x1", "x2", "x3")]), 1)
})

test_that("predict handles unseen factor levels and rejects bad data", {
  set.seed(2)
  data <- generateData(n = 600, scenario = "tree")
  fit <- suppressWarnings(glmtree(data[, c("x1", "x2", "x3")], data$y, K = 4, iterations = 15, ctree_controls = small_controls))
  new <- data[1:5, c("x1", "x2", "x3")]
  new$x1 <- factor(c("1", "2", "99", "3", "4"))
  pred <- predict(fit, new)
  expect_length(pred, 5)
  expect_false(anyNA(pred))
  # the unseen level gets the rate of positives of its segment
  expect_true(pred[3] %in% c(fit@best.tree$rates, fit@best.tree$global_rate))
  expect_error(predict(fit, data[, c("x1", "x2")]), "Missing columns")
  new$x2[1] <- NA
  expect_error(predict(fit, new), "missing")
})

test_that("the tree probabilities without newdata are those of the training data", {
  set.seed(3)
  d <- data.frame(f = factor(sample(letters[1:4], 500, TRUE)), z = rnorm(500))
  d$c <- factor(ifelse(d$f %in% c("a", "b") | d$z > 1.5, 1, 2))
  tree <- partykit::ctree(c ~ ., d, control = small_controls)
  expect_equal(unname(predict(tree, type = "prob")), unname(predict(tree, newdata = d[, c("f", "z")], type = "prob")))
})

test_that("partykit::glmtree objects (same class name, S3) still use their own methods", {
  set.seed(4)
  data <- generateData(n = 300, scenario = "tree")
  mob <- partykit::glmtree(y ~ x2 + x3 | x1, data = data, family = binomial)
  expect_length(predict(mob, data, type = "response"), 300)
  expect_output(print(mob), "Generalized linear model tree")
})
