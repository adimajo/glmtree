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

    if (scenario == "tree") {
      # We produce three clusters...
      n_clusters = 3
      data = matrix(0, nrow = n, ncol = 5)
      vraisemblance_generation = vector("numeric", length = n_clusters)

      # ... where y is generated from three different logistic regressions.
      theta = list()
      theta[[1]] = c(0, -1, 0.5)
      theta[[2]] = c(0, -0.5, 1.5)
      theta[[3]] = c(0, 1, -0.5)

      # For each observation, we calculate the subsequent log-likelihood and generate y
      for (c in seq(2, 2 * n_clusters, 2)) {
        x_cont = matrix(
          stats::rnorm(floor(n / n_clusters) * 2, mean = 0, sd = 1.5),
          nrow = floor(n / n_clusters),
          ncol = 2
        )
        x_cat = replicate(floor(n / n_clusters), sample(c(c - 1, c), 1, c(0.5, 0.5), replace =
                                                          TRUE))
        x = cbind(x_cat, x_cont)
        log_odd = apply(x_cont, 1, function(row)
          theta[[c / 2]][1] + t(theta[[c / 2]][2:3]) %*% row)
        y = stats::rbinom(floor(n / n_clusters), 1, 1 / (1 + exp(-log_odd)))
        data[(1 + (c / 2 - 1) * floor(n / n_clusters)):(c / 2 * floor(n /
                                                                        n_clusters)), ] <- cbind(x, y, rep(c / 2, floor(n / n_clusters)))
        vraisemblance_generation[c] <- sum(log_odd)
      }

      if (n - 3 * floor(n / n_clusters) > 0) {
        x_cont = matrix(
          stats::rnorm((n - 3 * floor(n / n_clusters)) * 2, mean = 0, sd = 1.5),
          nrow = n - 3 * floor(n / n_clusters),
          ncol = 2
        )
        x_cat = replicate(n - 3 * floor(n / n_clusters), sample(c(c - 1, c), 1, c(0.5, 0.5), replace =
                                                                  TRUE))
        x = cbind(x_cat, x_cont)
        log_odd = apply(x_cont, 1, function(row)
          theta[[c / 2]][1] + t(theta[[c / 2]][2:3]) %*% row)
        y = stats::rbinom(n - 3 * floor(n / n_clusters), 1, 1 / (1 + exp(-log_odd)))
        data[(3 * floor(n / n_clusters) + 1):n, ] <-
          cbind(x, y, rep(c / 2, n - 3 * floor(n / n_clusters)))
        vraisemblance_generation[c] <-
          vraisemblance_generation[c] + sum(log_odd)
      }

      # Everything is put in a dataframe
      data <- data.frame(data)
      colnames(data) <- c("x1", "x2", "x3", "y", "c")
      data$x1 <- as.factor(data$x1)

      if (visualize) {
        # graphics::par(mfrow=c(2,2))
        for (c in 1:n_clusters) {
          graphics::plot(
            data[data$c == c, 2],
            data[data$c == c, 3],
            col = data[data$c == c, 4] + 1,
            xlab = "First coordinate",
            ylab = "Second coordinate"
          )
        }
        # graphics::par(mfrow=c(1,1))
      }

      return(data)

    } else if (scenario == "no tree") {
      # We produce three "fake" clusters
      n_clusters = 3
      data = matrix(0, nrow = n, ncol = 3)
      vraisemblance_generation = vector("numeric", length = n_clusters)
      theta = c(3, 0.5, -1)

      for (c in 1:n_clusters) {
        x = matrix(
          stats::rnorm(
            floor(n / n_clusters) * 2,
            mean = 3 * c,
            sd = 1
          ),
          nrow = floor(n / n_clusters),
          ncol = 2
        )
        log_odd = apply(x, 1, function(row)
          theta[1] + t(theta[2:3]) %*% row)
        y = stats::rbinom(floor(n / n_clusters), 1, 1 / (1 + exp(-log_odd)))
        data[(1 + (c - 1) * floor(n / n_clusters)):(c * floor(n / n_clusters)), ] <-
          cbind(x, y)
        vraisemblance_generation[c] <- sum(log_odd)
      }

      if (n - 3 * floor(n / n_clusters) > 0) {
        x = matrix(
          stats::rnorm((n - 3 * floor(n / n_clusters)) * 2, mean = 3 * c, sd = 1),
          nrow = (n - 3 * floor(n / n_clusters)),
          ncol = 2
        )
        log_odd = apply(x, 1, function(row)
          theta[1] + t(theta[2:3]) %*% row)
        y = stats::rbinom(n - 3 * floor(n / n_clusters), 1, 1 / (1 + exp(-log_odd)))
        data[(3 * floor(n / n_clusters) + 1):n, ] <- cbind(x, y)
        vraisemblance_generation[c] <-
          vraisemblance_generation[c] + sum(log_odd)
      }

      data <- data.frame(data)
      colnames(data) <- c("x1", "x2", "y")

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

    } else
      stop(simpleError("Invalid scenario."))

  }
