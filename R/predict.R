#' @include allClasses.R
#' @importFrom methods show
NULL

#' Prediction of a logistic regression tree
#'
#' Applies the best tree to new observations to find their segment, then the logistic regression of that
#' segment. Observations with a factor level not seen in training receive the rate of positives of their
#' segment.
#' @exportMethod predict
#' @concept predict prediction
#' @docType methods
#' @name predict
#' @aliases predict
#' @description This defines the method "predict" for objects of S4 class \code{\link{glmtree-class}}.
#' @param ... Not used.
methods::setGeneric("predict")

#' @rdname predict
#' @aliases predict,glmtree-method
#' @param object The S4 \code{glmtree} object.
#' @param newdata A data frame with the features used to fit the tree.
#' @param type \code{"response"} for the predicted probability of the positive class, \code{"segment"}
#'   for the segment of each observation.
#' @return A numeric vector with one prediction per row of \code{newdata}.
#' @examples
#' set.seed(1)
#' data <- generateData(n = 300, scenario = "tree")
#' fit <- glmtree(
#'   x = data[, c("x1", "x2", "x3")], y = data$y, K = 5, iterations = 20,
#'   ctree_controls = partykit::ctree_control(alpha = 0.1, minbucket = 30, maxdepth = 3)
#' )
#' table(predict(fit, data[, c("x1", "x2", "x3")], type = "segment"))
methods::setMethod("predict", "glmtree", function(object, newdata, type = c("response", "segment"), ...) {
  # partykit::glmtree() objects are S3 objects of the same class name: leave them to their own method
  if (!isS4(object)) {
    return(methods::callNextMethod())
  }
  type <- match.arg(type)
  prediction <- predict_core(object@best.tree, newdata)
  if (type == "response") as.numeric(prediction$probability) else prediction$segment
})

#' Show a logistic regression tree
#'
#' @param object The S4 \code{glmtree} object.
#' @rdname glmtree-class
#' @aliases show,glmtree-method
#' @export
methods::setMethod("show", "glmtree", function(object) {
  cat("Logistic regression tree fitted by SEM\n")
  cat(
    "  segments:", length(object@best.tree$labels),
    "| best iteration:", object@performance$best_iteration, "of", length(object@performance$criterionEvolution), "\n"
  )
  cat("  ", object@parameters$criterion, " of the best tree: ", format(object@performance$performance, digits = 4),
    if (object@parameters$test) " (test set)" else if (object@parameters$validation) " (validation set)" else " (training set)",
    "\n",
    sep = ""
  )
  if (!is.null(object@best.tree$tree)) {
    print(object@best.tree$tree)
  }
  invisible(object)
})

# partykit::glmtree objects also have the S3 class "glmtree": make explicit print() calls use show().
#' @rdname glmtree-class
#' @param x The S4 \code{glmtree} object.
#' @param ... Not used.
#' @export
methods::setMethod("print", "glmtree", function(x, ...) {
  if (!isS4(x)) {
    return(methods::callNextMethod())
  }
  show(x)
})
