#' Random initialization of the segmentation
#'
#' This function randomly initializes the segmentation.
#' @param K The number of segments to start with (maximum number of segments there will be in the end).
#' @param n The number of observations to draw.
#' @return An integer vector of length n with values in 1 to K drawn with replacement.
#' @keywords internal
#' @author Adrien Ehrhardt

initialization <- function(K, n) {
  sample.int(K, n, replace = TRUE)
}
