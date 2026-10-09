#' Generates data from two logistic regression trees.
#'
#' This function generates data from two logistic regression trees: one with three apparent clusters (in terms of variance of the features) but a single logistic regression generating y | x, and one with a single apparent cluster but three different logistic regressions generating y | x given a categorical feature.
#' @param n The number of observations to draw.
#' @param scenario The "no tree" scenario denotes the first scenario where there is a single logistic regression generating the data. The "tree" scenario generates data from the second data generating mechanism where there are three logistic regressions.
#' @param visualize Whether (TRUE) or not (FALSE) to plot the generated data.
#' @return Generates and returns data according to a true logistic regression tree (if scenario = "tree") or a single regression tree (if scenario = "no tree"). Eventually plots this dataset (if visualize = TRUE).
#' @keywords glmtree
#' @author Adrien Ehrhardt
#' @export
#' @examples
#' generateData(scenario = "tree")
generateData <-
  function(n = 100,
           scenario = "tree",
           visualize = FALSE) {
    if (n < 10) {
      stop(simpleError("Needs more observations. Use at least n = 50."))
    }

    if (!is.logical(visualize)) {
      stop(simpleError("Parameter visualize needs to be logical."))
    }

    n_clusters <- 3
    # equal sized clusters, the remainder goes to the last one
    cluster <- c(rep(seq_len(n_clusters), each = n %/% n_clusters), rep(n_clusters, n %% n_clusters))

    if (scenario == "tree") {
      # Three clusters, each with its own pair of levels of x1 and its own logistic regression on (x2, x3)
      theta <- rbind(c(0, -1, 0.5), c(0, -0.5, 1.5), c(0, 1, -0.5))
      x1 <- 2 * cluster - 1 + stats::rbinom(n, 1, 0.5)
      x2 <- stats::rnorm(n, mean = 0, sd = 1.5)
      x3 <- stats::rnorm(n, mean = 0, sd = 1.5)
      log_odd <- theta[cluster, 1] + theta[cluster, 2] * x2 + theta[cluster, 3] * x3
      y <- stats::rbinom(n, 1, stats::plogis(log_odd))
      data <- data.frame(x1 = as.factor(x1), x2 = x2, x3 = x3, y = as.numeric(y), c = as.numeric(cluster))

      if (visualize) {
        for (c in seq_len(n_clusters)) {
          graphics::plot(
            data[data$c == c, 2],
            data[data$c == c, 3],
            col = data[data$c == c, 4] + 1,
            xlab = "First coordinate",
            ylab = "Second coordinate"
          )
        }
      }

      return(data)
    } else if (scenario == "no tree") {
      # Three "fake" clusters of the features, a single logistic regression
      theta <- c(3, 0.5, -1)
      x1 <- stats::rnorm(n, mean = 3 * cluster, sd = 1)
      x2 <- stats::rnorm(n, mean = 3 * cluster, sd = 1)
      y <- stats::rbinom(n, 1, stats::plogis(theta[1] + theta[2] * x1 + theta[3] * x2))
      data <- data.frame(x1 = x1, x2 = x2, y = as.numeric(y))

      if (visualize) {
        graphics::plot(
          data$x1,
          data$x2,
          col = data$y + 1,
          xlab = "First coordinate",
          ylab = "Second coordinate"
        )
      }

      return(data)
    } else {
      stop(simpleError("Invalid scenario."))
    }
  }
