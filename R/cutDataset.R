#' Split the observations into training, validation and test sets
#'
#' @param n The number of observations.
#' @param proportions Proportions of the test set and of the validation set: `c(test, validation)` when both
#'   are requested, a single proportion for whichever of the two is requested.
#' @param test Whether to build a test set.
#' @param validation Whether to build a validation set.
#' @return A list with the integer row indices `train`, `validation` and `test` (`NULL` when not requested).
#' @keywords internal
#' @author Adrien Ehrhardt
cutDataset <- function(n,
                       proportions,
                       test = TRUE,
                       validation = TRUE) {
  needed <- test + validation
  if (needed == 2) {
    if (length(proportions) < 2 || any(proportions[1:2] <= 0) || sum(proportions[1:2]) >= 1) {
      stop(simpleError("Argument proportions should contain 2 positive arguments which sum should be less than 1"))
    }
  } else if (needed == 1) {
    if (length(proportions) < 1 || proportions[1] <= 0 || proportions[1] >= 1) {
      stop(simpleError("Argument proportions should contain 1 argument strictly between 0 and 1"))
    }
  }
  shuffled <- sample.int(n, n)
  n_test <- if (test) floor(proportions[1] * n) else 0
  n_validation <- if (validation) floor(proportions[1 + test] * n) else 0
  list(
    train = shuffled[seq_len(n - n_test - n_validation) + n_test + n_validation],
    validation = if (validation) shuffled[n_test + seq_len(n_validation)],
    test = if (test) shuffled[seq_len(n_test)]
  )
}
