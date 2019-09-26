#' Class glmtree
#'
#' Class \code{glmtree} represents a logistic regression tree scheme associated with its optimal logistic regression models.
#'
#' @slot parameters The parameters associated with the method.
#' @slot best.tree The best discretization scheme found by the method given its parameters.
#' @slot performance The performance obtained with the method given its parameters.
#' @name glmdisc-class
#' @rdname glmdisc-class

methods::setClass(
  "glmtree",
  representation(
    parameters = "list",
    best.tree = "list",
    performance = "list"
  )
)
