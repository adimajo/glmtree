#' Calculating the normalized Gini index
#'
#' This function calculates the Gini index of a classification rule outputting probabilities. It is a classical metric in the context of Credit Scoring.
#' It is equal to 2 times the AUC (Area Under ROC Curve) minus 1, computed from the ranks of the predictions so that the result does not depend on the order of the observations when predictions are tied.
#' @param actual The numeric binary vector of the actual labels observed.
#' @param predicted The vector of the probabilities predicted by the classification rule.
#' @return The Gini index of the predicted probabilities as a single `num` (`NA` if `actual` has a single class).
#' @keywords gini index
#' @export
#' @author Adrien Ehrhardt
#' @examples
#' normalizedGini(c(1, 1, 1, 0, 0), c(0.7, 0.9, 0.5, 0.6, 0.3))
normalizedGini <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    stop("Actual and Predicted need to be equal lengths!")
  }
  positive <- actual == max(actual)
  n_positive <- sum(positive)
  n_negative <- length(actual) - n_positive
  if (n_positive == 0 || n_negative == 0) {
    return(NA_real_)
  }
  ranks <- rank(predicted)
  2 * ((sum(ranks[positive]) - n_positive * (n_positive + 1) / 2) / (n_positive * n_negative)) - 1
}
