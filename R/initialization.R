#' Random initialization of the segmentation
#'
#' This function randomy initializes the segmentation.
#' @param K The number of segments to start with (maximum number of segments there'll be in the end).
#' @param n The number of observations to draw.
#' @keywords internal
#' @author Adrien Ehrhardt

initialization <- function(K, n) {
  return(as.factor(sample.int(K, n, replace = TRUE)))
}
