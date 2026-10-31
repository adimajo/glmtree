# Internal helpers of glmtree().
#
# Segments are integer labels (never recoded between iterations). Logistic regressions are
# fitted with `glm.fit` on a design matrix built once, and probabilities of new observations are
# obtained by a coefficient lookup.

#' Check and normalize the features
#'
#' Accepts data frames (including tibbles) and matrices; characters and logicals become factors, integers
#' become numerics.
#' @param x The features.
#' @return A data frame with only numeric and factor columns.
#' @keywords internal
check_features <- function(x) {
  x <- as.data.frame(x)
  for (j in seq_along(x)) {
    if (is.character(x[[j]]) || is.logical(x[[j]])) x[[j]] <- factor(x[[j]])
    if (is.integer(x[[j]])) x[[j]] <- as.numeric(x[[j]])
  }
  if (!all(vapply(x, function(col) is.numeric(col) || is.factor(col), logical(1)))) {
    stop(simpleError("Unsupported data types. Columns of x must be numeric or factor."))
  }
  if (anyNA(x) || any(vapply(x, function(col) is.numeric(col) && any(is.infinite(col)), logical(1)))) {
    stop(simpleError("x must not contain missing or infinite values."))
  }
  x
}

#' Encode the response as 0/1
#'
#' The larger value (or the second level of a factor) is the positive class.
#' @param y A vector with two distinct values.
#' @return A numeric vector of 0 and 1.
#' @keywords internal
encode_response <- function(y) {
  if (anyNA(y)) {
    stop(simpleError("y must not contain missing values."))
  }
  if (is.factor(y)) y <- droplevels(y)
  if (nlevels(factor(y)) != 2) {
    stop(simpleError("y must be composed of two classes, either categorical or numerical."))
  }
  as.numeric(as.integer(factor(y)) - 1L)
}

#' Description of the features used to build the design matrix
#'
#' @param x The (checked) training features.
#' @return The names of the features, the levels seen in training of the factors and the factors that
#'   have at least two levels (the others carry no information).
#' @keywords internal
feature_description <- function(x) {
  levels <- lapply(x[vapply(x, is.factor, logical(1))], function(f) levels(droplevels(f)))
  list(
    names = names(x),
    levels = levels,
    informative = names(levels)[lengths(levels) >= 2]
  )
}

#' Align new features with the training features
#'
#' Factor levels not seen in training are set to `NA` (and flagged) so that trees and design matrices
#' can be applied to them.
#' @param x New features.
#' @param features Output of [feature_description()].
#' @return The aligned data frame and a logical vector flagging the rows with unseen levels.
#' @keywords internal
align_features <- function(x, features) {
  x <- check_features(x)
  missing_columns <- setdiff(features$names, names(x))
  if (length(missing_columns) > 0) {
    stop(simpleError(paste("Missing columns in the new data:", paste(missing_columns, collapse = ", "))))
  }
  x <- x[, features$names, drop = FALSE]
  unseen <- rep(FALSE, nrow(x))
  for (v in names(features$levels)) {
    values <- as.character(x[[v]])
    unseen <- unseen | !(values %in% features$levels[[v]])
    x[[v]] <- factor(values, levels = features$levels[[v]])
  }
  list(x = x, unseen = unseen)
}

#' Design matrix (intercept, numerics, treatment-coded factors), columns in the order of `x`
#' @param x Aligned features.
#' @param features Output of [feature_description()].
#' @return A numeric matrix.
#' @keywords internal
design_matrix <- function(x, features) {
  columns <- list(`(Intercept)` = rep(1, nrow(x)))
  for (v in features$names) {
    if (is.factor(x[[v]])) {
      if (v %in% features$informative) {
        for (level in features$levels[[v]][-1]) {
          columns[[paste0(v, level)]] <- as.numeric(x[[v]] %in% level)
        }
      }
    } else {
      columns[[v]] <- x[[v]]
    }
  }
  do.call(cbind, columns)
}

#' Logistic regression on a design matrix
#'
#' Coefficients of aliased columns are set to zero (they are redundant given the others).
#' @keywords internal
fit_logit <- function(X, y) {
  fit <- suppressWarnings(stats::glm.fit(X, y, family = stats::binomial()))
  coefficients <- fit$coefficients
  coefficients[is.na(coefficients)] <- 0
  list(coefficients = coefficients, rank = fit$rank)
}

#' One logistic regression per segment
#' @param seg Integer segment of each row.
#' @param labels Segments to fit.
#' @return The matrix of coefficients (one column per label) and the ranks.
#' @keywords internal
fit_segments <- function(X, y, seg, labels) {
  fits <- lapply(labels, function(l) {
    rows <- which(seg == l)
    fit_logit(X[rows, , drop = FALSE], y[rows])
  })
  list(
    B = do.call(cbind, lapply(fits, `[[`, "coefficients")),
    rank = vapply(fits, `[[`, numeric(1), "rank")
  )
}

#' Probability of the positive class given the segment of each row
#'
#' Rows whose segment has no logistic regression use the `fallback` coefficients.
#' @keywords internal
segment_probabilities <- function(X, seg, labels, B, fallback) {
  position <- match(seg, labels)
  has_model <- !is.na(position)
  eta <- numeric(nrow(X))
  eta[has_model] <- rowSums(X[has_model, , drop = FALSE] * t(B)[position[has_model], , drop = FALSE])
  if (!all(has_model)) {
    eta[!has_model] <- drop(X[!has_model, , drop = FALSE] %*% fallback)
  }
  stats::plogis(eta)
}

#' Segment of each row according to a tree (`NULL` is a tree with a single segment)
#' @param probabilities Optionally the already computed class probabilities of the tree.
#' @keywords internal
tree_segments <- function(tree, newdata, default, probabilities = NULL) {
  if (is.null(tree)) {
    return(rep(default, nrow(newdata)))
  }
  if (is.null(probabilities)) probabilities <- stats::predict(tree, newdata = newdata, type = "prob")
  as.integer(colnames(probabilities))[max.col(probabilities, ties.method = "first")]
}

#' Log-likelihood of Bernoulli predictions
#' @keywords internal
bernoulli_loglik <- function(y, p) {
  p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  y * log(p) + (1 - y) * log1p(-p)
}

#' Criterion (higher is better) of a segmentation on the data it was fitted on
#'
#' AIC and BIC are penalized per segment: the BIC of a segment uses the logarithm of its size.
#' @keywords internal
training_criterion <- function(criterion, y, p, seg, rank) {
  if (criterion == "gini") {
    return(normalizedGini(y, p))
  }
  loglik <- as.vector(rowsum(bernoulli_loglik(y, p), seg))
  size <- as.vector(rowsum(rep(1, length(y)), seg))
  penalty <- if (criterion == "aic") 2 * rank else log(size) * rank
  -sum(-2 * loglik + penalty)
}

#' Criterion (higher is better) on data not used for fitting: log-likelihood or Gini
#' @keywords internal
holdout_criterion <- function(criterion, y, p) {
  if (criterion == "gini") normalizedGini(y, p) else sum(bernoulli_loglik(y, p))
}

#' Draw one segment per row with probability proportional to `weights`
#'
#' Gumbel-max trick: vectorized categorical sampling.
#' @param weights Matrix of non-negative weights, one column per segment.
#' @param labels Segment of each column.
#' @keywords internal
draw_segments <- function(weights, labels) {
  noise <- -log(-log(matrix(stats::runif(length(weights)), nrow(weights))))
  labels[max.col(log(weights) + noise, ties.method = "first")]
}

#' Predictions of a fitted logistic regression tree on new features
#' @param model The `best.tree` slot of a `glmtree` object.
#' @param newdata New features.
#' @return A list with the predicted probabilities and the segments.
#' @keywords internal
predict_core <- function(model, newdata) {
  aligned <- align_features(newdata, model$features)
  X <- design_matrix(aligned$x, model$features)
  seg <- tree_segments(model$tree, aligned$x, default = model$labels[1])
  prob <- segment_probabilities(X, seg, model$labels, model$coefficients, model$global)
  if (any(aligned$unseen)) {
    # a factor level not seen in training: no regression can be applied, use the rate of the segment
    rate <- unname(model$rates[as.character(seg[aligned$unseen])])
    rate[is.na(rate)] <- model$global_rate
    prob[aligned$unseen] <- rate
  }
  list(probability = prob, segment = seg)
}
