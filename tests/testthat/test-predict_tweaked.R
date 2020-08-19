context("test-predict_tweaked")

test_that("predict_tweaked works", {
  data <- generateData(n = 100, scenario = "no tree")
  tree <- glmtree(x = data[, c("x1", "x2")], y = data$y, K = 5, iterations = 80, criterion = "aic")
  pred <- predict_tweaked(
    model = tree@best.tree$glms[[1]],
    df = data,
    c_iter = 1
  )
  expect_length(pred, 100)
  expect_true(all(pred <= 1))
  expect_true(all(pred >= 0))
})
