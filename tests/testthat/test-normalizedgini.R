test_that("normalizedGini works", {
  actual <- c(1, 1, 0, 0, 0)
  predicted <- c(0.8, 0.4, 0.5, 0.3, 0.2)
  expect_equal(normalizedGini(actual = actual, predicted = predicted), 2 * 5 / 6 - 1)
  expect_error(normalizedGini(c(1, 1, 1, 0, 0), c(0.7, 0.9, 0.5, 0.6)), "equal lengths")
})

test_that("normalizedGini does not depend on the order of tied predictions", {
  actual <- c(1, 0, 1, 0, 1, 0)
  predicted <- c(0.5, 0.5, 0.5, 0.5, 0.9, 0.1)
  reference <- normalizedGini(actual, predicted)
  # positives 0.5, 0.5, 0.9 against negatives 0.5, 0.5, 0.1: AUC = 7 / 9
  expect_equal(reference, 2 * 7 / 9 - 1)
  for (seed in 1:5) {
    set.seed(seed)
    shuffle <- sample(6)
    expect_equal(normalizedGini(actual[shuffle], predicted[shuffle]), reference)
  }
  expect_equal(normalizedGini(c(1, 0, 1, 0), rep(0.3, 4)), 0)
})

test_that("normalizedGini is 2 AUC - 1 and NA with a single class", {
  set.seed(1)
  actual <- rbinom(200, 1, 0.4)
  predicted <- runif(200) + 0.5 * actual
  auc <- mean(outer(predicted[actual == 1], predicted[actual == 0], ">")) + 0.5 * mean(outer(predicted[actual == 1], predicted[actual == 0], "=="))
  expect_equal(normalizedGini(actual, predicted), 2 * auc - 1)
  expect_true(is.na(normalizedGini(rep(1, 5), runif(5))))
})
