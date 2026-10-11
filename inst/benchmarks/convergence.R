# How does the quality of the selected tree depend on the number of SEM iterations, and how long
# does a fit take?
#
# Simulated data (generateData "tree", 3000 observations), 12 repetitions, 70% used for fitting.
# For each iteration budget, glmtree is fit with the BIC on the training data; the Gini is evaluated
# on the 30% held out. `patience = 25` stops the chain 25 iterations after the last improvement.
#
# Usage: Rscript inst/benchmarks/convergence.R [outdir]   (a few minutes)

outdir <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1L] else "benchmark-results"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
# partykit also exports a function called glmtree(): always call ours as glmtree::glmtree()
suppressMessages(library(glmtree))

gini <- function(y, p) {
  ranks <- rank(p)
  n1 <- sum(y)
  2 * ((sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * (length(y) - n1))) - 1
}

one_repetition <- function(rep) {
  set.seed(90000L + rep)
  data <- generateData(n = 3000, scenario = "tree")
  train <- sample(3000, 2100)
  features <- c("x1", "x2", "x3")
  controls <- partykit::ctree_control(alpha = 0.1, minbucket = 100, maxdepth = 4)
  out <- NULL
  for (iter in c(25, 50, 100, 200, 400, 800)) {
    for (patience in c(NA, 25)) {
      set.seed(rep)
      elapsed <- system.time(
        fit <- glmtree::glmtree(data[train, features], data$y[train],
          K = 5, iterations = iter, burn_in = 10,
          patience = if (is.na(patience)) NULL else patience, ctree_controls = controls
        )
      )[["elapsed"]]
      out <- rbind(out, data.frame(
        rep = rep, iterations = iter, patience = if (is.na(patience)) "none" else as.character(patience),
        test_gini = gini(data$y[-train], predict(fit, data[-train, features])), secs = elapsed,
        iterations_done = length(fit@performance$criterionEvolution),
        best_iteration = fit@performance$best_iteration,
        segments = length(fit@best.tree$labels)
      ))
    }
  }
  out
}

results <- do.call(rbind, parallel::mclapply(1:12, one_repetition,
  mc.cores = min(6L, parallel::detectCores()), mc.set.seed = FALSE
))
write.csv(results, file.path(outdir, "convergence.csv"), row.names = FALSE)
summary_table <- aggregate(cbind(test_gini, secs, iterations_done, best_iteration, segments) ~ iterations + patience,
  results, mean
)
summary_table <- summary_table[order(summary_table$patience, summary_table$iterations), ]
print(summary_table, digits = 3, row.names = FALSE)
