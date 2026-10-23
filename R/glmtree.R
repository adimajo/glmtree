#' Logistic regression tree by Stochastic-Expectation-Maximization
#'
#' This function produces a logistic regression tree: a decision tree with logistic regressions at its leaves.
#' The segmentation of the observations is a latent variable: at each iteration a logistic regression is
#' fitted in each segment, a conditional inference tree (\code{partykit::ctree}) is fitted to the current
#' segmentation, and each observation is re-assigned to a segment drawn proportionally to the tree
#' probability times the likelihood of its label under the segment's regression. A few good trees are
#' kept along the chain and the best one is chosen according to \code{criterion}.
#' @param x The features to use for prediction (data frame or matrix with numeric, integer and factor
#'   columns; characters and logicals are converted to factors). No missing values are allowed.
#' @param y The binary / boolean labels to predict. The larger value (or the second level of a factor) is
#'   the positive class.
#' @param K The number of segments to start with (maximum number of segments there will be in the end).
#' @param iterations The number of iterations to do in the SEM algorithm (default: 200).
#' @param test Boolean : True if a test set should be held out, on which the \code{criterion} of the best
#'   tree is reported in \code{performance} (default: FALSE).
#' @param validation Boolean : True if a validation set should be held out, on which the \code{criterion}
#'   is computed at each iteration to choose the best tree (default: FALSE, the criterion is then computed on
#'   the training set).
#' @param proportions The proportions of observations held out: \code{c(test, validation)} when both are
#'   requested, otherwise a single proportion for whichever is requested. Not used when both are FALSE.
#'   Errors when invalid (e.g. a sum greater than one). Default: 0.3.
#' @param criterion The criterion (`"gini"`, `"aic"`, `"bic"`) used to choose the best tree among the generated
#'   ones (default: `"bic"`). AIC and BIC are summed over segments (the BIC penalty of a segment uses the
#'   logarithm of its size) and are replaced by the log-likelihood when computed on held out data, where no
#'   penalty is needed. The Gini index is best used with a validation set.
#' @param ctree_controls The controls to use for \code{partykit::ctree}.
#' @param burn_in The number of first iterations that cannot be chosen as the best tree (default: the
#'   minimum of 50 and a quarter of \code{iterations}).
#' @param patience If not \code{NULL}, the chain stops when the criterion did not improve for
#'   \code{patience} iterations (after the burn-in).
#' @param verbose Whether to print the criterion at each iteration.
#' @return An S4 object of class \code{\link{glmtree-class}} that contains the parameters used to search for the
#'   logistic regression tree, the best tree it found, and its performance. Use \code{\link{predict}}
#'   to predict on new data.
#' @keywords glmtree
#' @author Adrien Ehrhardt
#' @export
#' @examples
#' set.seed(1)
#' data <- generateData(n = 300, scenario = "tree")
#' fit <- glmtree(
#'   x = data[, c("x1", "x2", "x3")], y = data$y, K = 5, iterations = 20,
#'   ctree_controls = partykit::ctree_control(alpha = 0.1, minbucket = 30, maxdepth = 3)
#' )
#' fit
#' head(predict(fit, data[, c("x1", "x2", "x3")]))
glmtree <-
  function(x,
           y,
           K = 10,
           iterations = 200,
           test = FALSE,
           validation = FALSE,
           proportions = c(0.3),
           criterion = "bic",
           ctree_controls = partykit::ctree_control(
             alpha = 0.1,
             minbucket = 100,
             maxdepth = 5
           ),
           burn_in = min(50L, iterations %/% 4L),
           patience = NULL,
           verbose = FALSE) {
    # Checks
    if (length(y) != NROW(x)) {
      stop(simpleError("x and y must be of same length!"))
    }
    x <- check_features(x)
    y <- encode_response(y)

    if (!criterion %in% c("gini", "bic", "aic")) {
      stop(paste("Criterion", criterion, "not supported"))
    }
    if (!is.numeric(K) || length(K) != 1 || K < 2) {
      stop(simpleError("K must be a number greater or equal to 2."))
    }
    if (!is.numeric(iterations) || length(iterations) != 1 || iterations < 2) {
      stop(simpleError("iterations must be a number greater or equal to 2."))
    }
    if (!is.numeric(burn_in) || length(burn_in) != 1 || burn_in < 0 || burn_in >= iterations) {
      stop(simpleError("burn_in must be a number greater or equal to 0 and less than iterations."))
    }
    if (!is.null(patience) && (!is.numeric(patience) || length(patience) != 1 || patience < 1)) {
      stop(simpleError("patience must be NULL or a number greater or equal to 1."))
    }
    if ((criterion == "gini") & (validation == FALSE)) {
      warning("Using Gini index on training set might yield an overfitted model.")
    }
    if ((criterion %in% c("aic", "bic")) & (validation == TRUE)) {
      warning(
        "No need to penalize the log-likelihood when a validation set is used. Using log-likelihood instead of AIC/BIC."
      )
    }

    # Initialization: training, validation and test sets
    split <- cutDataset(nrow(x), proportions, test = test, validation = validation)
    x_train <- droplevels(x[split$train, , drop = FALSE])
    y_train <- y[split$train]
    n <- nrow(x_train)
    features <- feature_description(x_train)
    X <- design_matrix(x_train, features)
    held_out <- function(rows) {
      if (is.null(rows)) {
        return(NULL)
      }
      aligned <- align_features(x[rows, , drop = FALSE], features)
      list(x = aligned$x, X = design_matrix(aligned$x, features), y = y[rows])
    }
    validation_set <- held_out(split$validation)
    test_set <- held_out(split$test)
    global <- fit_logit(X, y_train)$coefficients

    c_hat <- initialization(K, n)
    c_map <- c_hat
    link <- NULL
    criterion_iter <- rep(NA_real_, iterations)
    best <- NULL

    # SEM
    for (i in seq_len(iterations)) {
      # p(y | x, c_hat): regressions in the current segments and their predictions on all rows
      labels_hat <- sort(unique(c_hat))
      fit_hat <- fit_segments(X, y_train, c_hat, labels_hat)

      # p(y | x, c_map): regressions in the segments given by the tree, and the criterion
      if (identical(c_map, c_hat)) {
        labels_map <- labels_hat
        fit_map <- fit_hat
      } else {
        labels_map <- sort(unique(c_map))
        fit_map <- fit_segments(X, y_train, c_map, labels_map)
      }
      if (is.null(validation_set)) {
        p_map <- segment_probabilities(X, c_map, labels_map, fit_map$B, global)
        criterion_iter[i] <- training_criterion(criterion, y_train, p_map, c_map, fit_map$rank)
      } else if (!is.null(link)) {
        seg_validation <- tree_segments(link, validation_set$x, labels_map[1])
        p_validation <- segment_probabilities(validation_set$X, seg_validation, labels_map, fit_map$B, global)
        criterion_iter[i] <- holdout_criterion(criterion, validation_set$y, p_validation)
      }
      if (verbose) {
        message("The ", criterion, " criterion for iteration ", i, " is ", criterion_iter[i])
      }

      # The tree used here (the one of the previous iteration) is the one that produced c_map
      current <- list(iteration = i, c_map = c_map, link = link, criterion = criterion_iter[i])
      if (i >= 2 && i > burn_in && !is.na(criterion_iter[i]) &&
        (is.null(best) || criterion_iter[i] > best$criterion)) {
        best <- current
      }
      if (!is.null(patience) && !is.null(best) && i - best$iteration >= patience) {
        break
      }

      # p(c_hat | x): tree, then the new c_map and a draw of the new c_hat
      if (length(labels_hat) < 2) {
        break
      }
      data_tree <- data.frame(.segment = factor(c_hat), x_train)
      link <- partykit::ctree(.segment ~ ., data = data_tree, control = ctree_controls)
      probabilities <- stats::predict(link, type = "prob")
      c_map <- tree_segments(link, x_train, labels_hat[1], probabilities)
      p_hat <- stats::plogis(X %*% fit_hat$B)
      likelihood <- y_train * p_hat + (1 - y_train) * (1 - p_hat)
      weights <- probabilities * pmax(likelihood, 1e-12)
      c_hat <- draw_segments(weights, as.integer(colnames(probabilities)))
    }
    # Chains that collapsed to a single segment before the burn-in: keep the last state
    if (is.null(best)) {
      best <- current
    }
    iterations_done <- i

    # Final models: one logistic regression per segment of the best iteration
    labels <- sort(unique(best$c_map))
    fit_best <- fit_segments(X, y_train, best$c_map, labels)
    # `glm` objects for inspection, on the same design columns (so same coefficients as used to predict)
    design_data <- as.data.frame(X[, -1, drop = FALSE])
    names(design_data) <- make.names(colnames(X)[-1], unique = TRUE)
    glms <- lapply(labels, function(l) {
      rows <- best$c_map == l
      data_segment <- data.frame(.response = y_train[rows], design_data[rows, , drop = FALSE])
      suppressWarnings(stats::glm(.response ~ ., data = data_segment, family = stats::binomial()))
    })
    names(glms) <- labels
    colnames(fit_best$B) <- labels
    best_tree <- list(
      tree = best$link,
      glms = glms,
      coefficients = fit_best$B,
      labels = labels,
      features = features,
      global = global,
      rates = tapply(y_train, best$c_map, mean),
      global_rate = mean(y_train)
    )

    performance <- criterion_iter[best$iteration]
    if (!is.null(test_set)) {
      p_test <- predict_core(best_tree, test_set$x)$probability
      performance <- holdout_criterion(criterion, test_set$y, p_test)
    }

    methods::new(
      Class = "glmtree",
      parameters = list(
        K = K,
        iterations = iterations,
        test = test,
        validation = validation,
        proportions = proportions,
        criterion = criterion,
        ctree_controls = ctree_controls,
        burn_in = burn_in,
        patience = patience,
        sizes = c(
          train = length(split$train),
          validation = length(split$validation),
          test = length(split$test)
        )
      ),
      best.tree = best_tree,
      performance = list(
        performance = performance,
        criterionEvolution = as.list(criterion_iter[seq_len(iterations_done)]),
        best_iteration = best$iteration
      )
    )
  }
