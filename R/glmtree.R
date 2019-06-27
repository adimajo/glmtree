#' Logistic regression tree by Stochastic-Expectation-Maximization
#'
#' This function produces a logistic regression tree: a decision tree with logistic regressions at its leaves.
#' @param x The features to use for prediction.
#' @param y The binary / boolean labels to predict.
#' @param K The number of segments to start with (maximum number of segments there'll be in the end).
#' @param iterations The number of iterations to do in the SEM protocole (default: 200).
#' @param test Boolean : True if the algorithm should use predictors to construct a test set on which to calculate the provided criterion using the best discretization scheme (chosen thanks to the provided criterion on either the test set (if true) or the training set (otherwise)) (default: TRUE).
#' @param validation Boolean : True if the algorithm should use predictors to construct a validation set on which to search for the best discretization scheme using the provided criterion (default: TRUE).
#' @param proportions The list of the proportions wanted for test and validation set. Not used when both test and validation are false. Only the first is used when there is only one of either test or validation that is set to TRUE. Produces an error when the sum is greater to one. Default: list(0.2,0.2) so that the training set has 0.6 of the input observations.
#' @keywords glmtree
#' @author Adrien Ehrhardt
#' @export
#' @examples
#' data = generateData(n = 1000, scenario = "no tree")
#' glmtree(data[,c("x1", "x2")], data$y)

glmtree <- function(x, y = y, K = 10, iterations = 200, test=F, validation=T, proportions = c(0.3)) {

  # Checks
  if (!length(y)==length(x[,1])) {stop(simpleError("x and y must be of same length!"))}

  if (!nlevels(factor(y)) == 2) {stop(simpleError("y must be composed of two classes, either categorical or numerical."))}

  types_data <- sapply(x[1,], class)

  if (sum(!(types_data %in% c("numeric","factor")))>0) {
    stop(simpleError("Unsupported data types. Columns of x must be numeric or factor."))
  }

  # Initialization
  # Obtain training, test and validation datasets.
  n = nrow(x)
  current_best = 1

  ensemble <- cutDataset(n,proportions,test=test,validation=validation)
  ensemble[[1]] = 1:n %in% ensemble[[1]]
  ensemble[[2]] = 1:n %in% ensemble[[2]]
  ensemble[[3]] = 1:n %in% ensemble[[3]]

  criterion_iter=list()
  df = data.frame(x, y, c_map = initialization(K,nrow(x)))
  df$c_hat = df$c_map

  # Tirage SEM
  for (i in 1:iterations) {
    reglogs_c_hat = list()
    reglogs_c_map = list()
    predictions_log <- matrix(0, nrow = n, ncol = nlevels(df$c_hat))

    # Apprentissage des régressions p(y | x, c_hat) et remplissage des probabilités prédites
    for (c_iter in 1:nlevels(df$c_hat)) {
      reglogs_c_hat[[c_iter]] <- stats::glm(y ~ ., data = df[df$c_hat==levels(df$c_hat)[c_iter],c("y",colnames(x))], family=stats::binomial(link="logit"))
      predictions_log[,c_iter] <- stats::predict(reglogs_c_hat[[c_iter]], df, type="response")
    }

    # Apprentissage des régressions p(y | x, c_map) et calcul de l'AIC "cumulé"
    criterion_iter[[i]] = -Inf

    for (c_iter in 1:nlevels(df$c_map)) {
      reglogs_c_map[[c_iter]] <- stats::glm(y ~ ., data = df[df$c_map==levels(df$c_map)[c_iter],c("y",colnames(x))], family=stats::binomial(link="logit"))
      criterion_iter[[i]] = criterion_iter[[i]] - reglogs_c_map[[c_iter]]$aic
    }

    # Mise à jour éventuelle de la meilleure solution actuelle (+burn-in de 100 itérations)
    if (i>=100 & criterion_iter[[i]] >= criterion_iter[[current_best]]) {
      best_reglogs = reglogs_c_map
      best_link = tryCatch(link,error=function(cond) list())
      current_best = i
    }

    # Apprentissage du lien p(c_hat | x)
    link = rpart::rpart(c_hat ~ ., data = df[,c("c_hat",colnames(x))], method = "class")
    link = rpart::prune(link,cp= 0.1)

    # Calcul de c_map
    df$c_map <- factor(apply(stats::predict(link,df),1,function(p) names(which.max(p))))

    # Tirage du nouveau c_hat
    p <- prop.table(stats::predict(link,df)*(df$y*predictions_log + (1-df$y)*(1-predictions_log)),1)
    df$c_hat <- factor(apply(p,1,function(row) sample(levels(df$c_hat),1,prob = row)))
  }

  return(best_link)
}
