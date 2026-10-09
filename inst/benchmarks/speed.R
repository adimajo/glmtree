# Time and accuracy of one installed version of glmtree: run it once per version to compare, e.g.
#   Rscript inst/benchmarks/speed.R results/speed.csv                 # the version on the default library path
#   R_LIBS=/path/to/library-with-0.3.1 Rscript inst/benchmarks/speed.R results/speed.csv
# Results are appended to the csv, with the version of the package.
#
# 50 SEM iterations, K = 5, BIC on the training data, "tree" scenario, 70% used for fitting,
# 5 repetitions for each sample size. Sequential: timings are not distorted by parallel runs.

out <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1L] else "speed.csv"
# partykit also exports a function called glmtree(): always call ours as glmtree::glmtree()
suppressMessages(library(glmtree))
version <- as.character(utils::packageVersion("glmtree"))
new_api <- exists("predict_core", asNamespace("glmtree"))

gini <- function(y, p) {
  ranks <- rank(p)
  n1 <- sum(y)
  2 * ((sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * (length(y) - n1))) - 1
}

# Version 0.3.1 has no predict method: segment by the tree, then the regression of the segment.
# Its list of regressions is indexed by the position of the segment among the segments that the tree
# gives to the training data (as in the old vignette).
predict_old <- function(fit, newdata, train_x) {
  segment_of <- function(x) {
    probabilities <- predict(fit@best.tree$tree, x, type = "prob")
    colnames(probabilities)[max.col(probabilities, ties.method = "first")]
  }
  train_levels <- levels(factor(segment_of(train_x)))
  segment <- match(segment_of(newdata), train_levels)
  p <- numeric(nrow(newdata))
  for (j in unique(segment)) {
    p[segment == j] <- suppressWarnings(predict(fit@best.tree$glms[[j]], newdata[segment == j, ], type = "response"))
  }
  p
}

results <- NULL
for (n in c(1000, 3000, 10000)) {
  for (rep in 1:5) {
    set.seed(100 * rep)
    data <- generateData(n = n, scenario = "tree")
    train <- sample(n, floor(0.7 * n))
    features <- c("x1", "x2", "x3")
    controls <- partykit::ctree_control(alpha = 0.1, minbucket = floor(0.05 * length(train)), maxdepth = 4)
    set.seed(rep)
    elapsed <- system.time(fit <- suppressWarnings(suppressMessages(
      glmtree::glmtree(data[train, features], data$y[train], K = 5, iterations = 50, criterion = "bic", ctree_controls = controls)
    )))[["elapsed"]]
    p <- if (new_api) predict(fit, data[-train, features]) else predict_old(fit, data[-train, features], data[train, features])
    results <- rbind(results, data.frame(
      version = version, n = n, rep = rep, secs = elapsed, secs_per_iteration = elapsed / 50,
      test_gini = gini(data$y[-train], p), segments = length(fit@best.tree$glms)
    ))
  }
}
utils::write.table(results, out, sep = ",", row.names = FALSE, col.names = !file.exists(out), append = file.exists(out))
print(aggregate(cbind(secs, secs_per_iteration, test_gini, segments) ~ version + n, results, mean), digits = 3)
