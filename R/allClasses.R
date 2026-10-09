#' Class glmtree
#'
#' Class \code{glmtree} represents a logistic regression tree scheme associated with its optimal logistic regression models.
#'
#' @slot parameters The parameters associated with the method.
#' @slot best.tree The best tree found by the method given its parameters: `tree` (the `partykit::ctree`
#'   giving the segment of an observation, `NULL` if the chain collapsed to a single segment), `glms` (the
#'   logistic regression of each segment, a named list of `glm` objects) and the elements needed to predict
#'   on new data.
#' @slot performance The performance obtained with the method given its parameters: `performance` (the
#'   criterion of the best iteration, computed on the test set if there is one), `criterionEvolution` (the
#'   criterion at each iteration) and `best_iteration`.
#' @name glmtree-class
#' @rdname glmtree-class
#' @exportClass glmtree

methods::setClass(
  "glmtree",
  representation(
    parameters = "list",
    best.tree = "list",
    performance = "list"
  )
)
