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
#' @param criterion The criterion ('gini','aic','bic') to use to choose the best discretization scheme among the generated ones (default: 'gini'). Nota Bene: it is best to use 'gini' only when test is set to TRUE and 'aic' or 'bic' when it is not. When using 'aic' or 'bic' with a test set, the likelihood is returned as there is no need to penalize for generalization purposes.
#' @param ctree_controls The controls to use for `partykit::ctree`.
#' @return An S4 object of class `glmtree` that contains the parameters used to search for the logistic regression tree, the best tree it found, and its performance.
#' @keywords glmtree
#' @importFrom magrittr "%>%"
#' @author Adrien Ehrhardt
#' @export
#' @examples
#' data <- generateData(n = 100, scenario = "no tree")
#' glmtree(x = data[, c("x1", "x2")], y = data$y, K = 5, iterations = 80, criterion = "aic")
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
           )) {
    # Checks
    if (!length(y) == length(x[, 1])) {
      stop(simpleError("x and y must be of same length!"))
    }

    if (!nlevels(factor(y)) == 2) {
      stop(simpleError(
        "y must be composed of two classes, either categorical or numerical."
      ))
    }

    types_data <- sapply(x[1, ], class)

    if (sum(!(types_data %in% c("numeric", "factor"))) > 0) {
      stop(simpleError(
        "Unsupported data types. Columns of x must be numeric or factor."
      ))
    }

    if (!criterion %in% c("gini", "bic", "aic")) {
      stop(paste("Criterion", criterion, "not supported"))
    }

    if ((criterion == "gini") & (validation == FALSE)) {
      warning("Using Gini index on training set might yield an overfitted model.")
    }

    if ((criterion %in% c("aic", "bic")) & (validation == TRUE)) {
      warning(
        "No need to penalize the log-likelihood when a validation set is used. Using log-likelihood instead of AIC/BIC."
      )
    }

    # Initialization
    # Obtain training, test and validation datasets.
    n <- nrow(x)
    current_best <- 1

    ensemble <-
      cutDataset(n, proportions, test = test, validation = validation)
    ensemble[[1]] <- 1:n %in% ensemble[[1]]
    ensemble[[2]] <- 1:n %in% ensemble[[2]]
    ensemble[[3]] <- 1:n %in% ensemble[[3]]

    criterion_iter <- list(-Inf)
    df <- data.frame(x, y, c_map = initialization(K, nrow(x)))
    df$c_hat <- df$c_map

    # Tirage SEM
    for (i in 1:iterations) {
      reglogs_c_hat <- list()
      reglogs_c_map <- list()
      predictions_log <- matrix(0, nrow = n, ncol = nlevels(df$c_hat))

      # Apprentissage des régressions p(y | x, c_hat) et remplissage des probabilités prédites
      for (c_iter in 1:nlevels(df$c_hat)) {
        data_train <- df[df$c_hat == levels(df$c_hat)[c_iter], c("y", colnames(x))]
        data_train <- data_train[sapply(data_train, function(x) {
          !is.factor(x) | nlevels(factor(x)) > 1
        })]

        reglogs_c_hat[[c_iter]] <-
          stats::glm(y ~ .,
            data = data_train,
            family = stats::binomial(link = "logit")
          )
        predictions_log[, c_iter] <-
          predict_tweaked(
            model = reglogs_c_hat[[c_iter]],
            df = df,
            c_iter = c_iter
          )
      }
      predictions_log[is.nan(predictions_log)] <- 0

      # Apprentissage des régressions p(y | x, c_map) et calcul de l'AIC "cumulé"
      criterion_iter[[i]] <- 0

      for (c_iter in 1:nlevels(df$c_map)) {
        data_train_c_map <- df[df$c_map == levels(df$c_map)[c_iter], c("y", colnames(x))]
        data_train_c_map <- data_train_c_map[sapply(data_train_c_map, function(x) {
          !is.factor(x) | nlevels(factor(x)) > 1
        })]

        reglogs_c_map[[c_iter]] <-
          stats::glm(y ~ .,
            data = data_train_c_map,
            family = stats::binomial(link = "logit")
          )

        if (i >= 2) {
          if (criterion == "aic") {
            if (!validation) {
              criterion_iter[[i]] <- criterion_iter[[i]] - reglogs_c_map[[c_iter]]$aic
            } else {
              criterion_iter[[i]] <- criterion_iter[[i]] + sum(log(
                y[ensemble[[2]] %in% 1:n &
                  df$c_map == levels(df$c_map)[c_iter]] * stats::predict(reglogs_c_map[[c_iter]],
                  newdata = df[ensemble[[2]] %in% 1:n &
                    df$c_map == levels(df$c_map)[c_iter], ],
                  type = "response"
                ) + (1 - y[ensemble[[2]]]) *
                  (1 - y[ensemble[[2]]] * stats::predict(reglogs_c_map[[c_iter]],
                    newdata = df[ensemble[[2]], ],
                    type = "response"
                  ))
              ))
            }
          } else if (criterion == "bic") {
            if (!validation) {
              criterion_iter[[i]] <- criterion_iter[[i]] + 2 * stats::logLik(reglogs_c_map[[c_iter]]) -
                log(length(ensemble[[1]] %in% df$c_map == levels(df$c_map)[c_iter])) * length(reglogs_c_map[[c_iter]]$coefficients)
            } else {
              criterion_iter[[i]] <- criterion_iter[[i]] + sum(log(
                y[ensemble[[2]]] * stats::predict(
                  reglogs_c_map[[c_iter]],
                  newdata = df[ensemble[[2]] %in% 1:n &
                    df$c_map == levels(df$c_map)[c_iter], ],
                  type = "response"
                ) + (1 - y[ensemble[[2]] %in% 1:n &
                  df$c_map == levels(df$c_map)[c_iter]]) * (
                  1 - y[ensemble[[2]] %in% 1:n &
                    df$c_map == levels(df$c_map)[c_iter]] * stats::predict(
                    reglogs_c_map[[c_iter]],
                    newdata = df[ensemble[[2]] %in% 1:n &
                      df$c_map == levels(df$c_map)[c_iter], ],
                    type = "response"
                  )
                )
              ))
            }
          } else if (!validation) {
            criterion_iter[[i]] <- rbind(
              criterion_iter[[i]],
              cbind(reglogs_c_map[[c_iter]]$y, reglogs_c_map[[c_iter]]$fitted.values)
            )
          } else {
            criterion_iter[[i]] <- rbind(criterion_iter[[i]], cbind(
              y[ensemble[[2]] %in% 1:n],
              stats::predict(reglogs_c_map[[c_iter]], newdata = df[ensemble[[2]] %in% 1:n &
                df$c_map == levels(df$c_map)[c_iter], ], type = "response")
            ))
          }
        }
      }


      if (class(criterion_iter[[i]]) == "matrix") {
        criterion_iter[[i]] <- normalizedGini(
          actual = criterion_iter[[i]][, 1],
          predicted = criterion_iter[[i]][, 2]
        )
      }

      message("The ", criterion, " criterion for iteration ", i, " is ", criterion_iter[[i]])

      # Mise à jour éventuelle de la meilleure solution actuelle (+burn-in de 100 itérations)
      if (i == 2 | (i >= 50 &
        criterion_iter[[i]] > criterion_iter[[current_best]])) {
        best_reglogs <- reglogs_c_map
        best_link <- tryCatch(
          link,
          error = function(cond) {
            list()
          }
        )
        current_best <- i
      }

      # Apprentissage du lien p(c_hat | x)
      # TODO: laisser le choix à l'utilisateur de la méthode d'arbre à utiliser
      # TODO: implémenter l'arbre avec caret::train(..., method = "ctree") pour pruning automatique
      # link = rpart::rpart(c_hat ~ ., data = df[,c("c_hat",colnames(x))], method = "class")
      # link = rpart::prune(link,cp = link$cptable[which.min(link$cptable[,"xerror"]),1])
      if (nlevels(factor(df$c_hat)) > 1) {
        link <- partykit::ctree(
          c_hat ~ .,
          data = df[, c("c_hat", colnames(x))] %>% dplyr::mutate_at("c_hat", as.factor),
          control = ctree_controls
        )
      } else {
        break
      }
      # link = caret::train(c_hat ~ ., data = df[,c("c_hat",colnames(x))] %>% dplyr::mutate_at("c_hat", as.factor), method = "ctree")

      # Calcul de c_map
      # df$c_map <- factor(apply(stats::predict(link,df),1,function(p) names(which.max(p))))
      df$c_map <-
        factor(apply(stats::predict(link, df, type = "prob"), 1, function(p) {
          names(which.max(p))
        }))

      # Tirage du nouveau c_hat
      # p <- prop.table(stats::predict(link,df)*(df$y*predictions_log + (1-df$y)*(1-predictions_log)),1)
      p <-
        prop.table(
          stats::predict(link, df, type = "prob") * (df$y * predictions_log + (1 -
            df$y) * (1 - predictions_log)),
          1
        )
      df$c_hat <-
        factor(apply(p, 1, function(row) {
          sample(levels(df$c_hat), 1, prob = row)
        }))
    }

    return(
      methods::new(
        Class = "glmtree",
        parameters = list(
          K = K,
          iterations = iterations,
          test = test,
          validation = validation,
          proportions = proportions,
          criterion = criterion,
          ctree_controls = ctree_controls
        ),
        best.tree = list(tree = best_link, glms = best_reglogs),
        performance = list(performance = criterion_iter[[current_best]], criterionEvolution = criterion_iter)
      )
    )
  }
