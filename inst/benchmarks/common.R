# Shared pieces of the benchmark scripts: datasets, competitors and the repetition runner.
# Sourced by real_data.R (which sets `sem_iter` first).

suppressMessages({
  library(glmtree)
  library(partykit)
})

# ---- datasets -------------------------------------------------------------------------

as_binary <- function(x, positive) as.integer(x == positive)

load_data <- function(name, package) {
  environment <- new.env()
  utils::data(list = name, package = package, envir = environment)
  environment[[name]]
}

load_datasets <- function() {
  german <- load_data("germancredit", "scorecard")
  pima <- rbind(load_data("Pima.tr", "MASS"), load_data("Pima.te", "MASS"))
  default <- load_data("Default", "ISLR")
  credit_card <- load_data("CreditCard", "AER")
  caravan <- load_data("Caravan", "ISLR")
  simulate <- function(scenario) {
    function(rep) {
      set.seed(90000L + rep)
      data <- generateData(n = 3000L, scenario = scenario)
      list(X = data[, setdiff(names(data), c("y", "c"))], y = as.integer(data$y))
    }
  }
  list(
    sim_tree = list(
      description = "Simulation (generateData 'tree'): 3 segments, each with its own logistic regression, n = 3000",
      generate = simulate("tree")
    ),
    sim_notree = list(
      description = "Simulation (generateData 'no tree'): 3 clusters of features, a single logistic regression, n = 3000",
      generate = simulate("no tree")
    ),
    german = list(
      description = "German credit (UCI Statlog): 1000 loans, 7 numeric + 13 categorical features",
      X = german[, setdiff(names(german), "creditability")],
      y = as_binary(german$creditability, "bad")
    ),
    pima = list(
      description = "Pima Indians diabetes (MASS::Pima.tr + Pima.te): 532 women, 7 numeric features",
      X = pima[, setdiff(names(pima), "type")],
      y = as_binary(pima$type, "Yes")
    ),
    default = list(
      description = "ISLR::Default, simulated credit card default: 10000 customers, 3 features",
      X = default[, c("student", "balance", "income")],
      y = as_binary(default$default, "Yes")
    ),
    creditcard = list(
      description = "AER::CreditCard: 1319 card applications (share and expenditure removed: they leak the outcome)",
      X = credit_card[, setdiff(names(credit_card), c("card", "share", "expenditure"))],
      y = as_binary(credit_card$card, "yes")
    ),
    caravan = list(
      description = "ISLR::Caravan: 5822 insurance customers, 85 features, 6% positives (purchase of a caravan policy)",
      X = caravan[, setdiff(names(caravan), "Purchase")],
      y = as_binary(caravan$Purchase, "Yes")
    )
  )
}

# ---- helpers ----------------------------------------------------------------------------

gini <- function(y, p) {
  ranks <- rank(p)
  n1 <- sum(y)
  2 * ((sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * (length(y) - n1))) - 1
}

clean_types <- function(X) {
  for (j in seq_along(X)) if (is.character(X[[j]]) || is.logical(X[[j]])) X[[j]] <- factor(X[[j]])
  X
}

# Factor levels of `test` as in `train` (unseen levels -> most frequent level); drops unused levels.
align_factors <- function(train, test) {
  for (j in seq_along(train)) {
    if (is.factor(train[[j]])) {
      mode_level <- names(which.max(table(train[[j]])))
      value <- as.character(test[[j]])
      value[!(value %in% levels(train[[j]]))] <- mode_level
      test[[j]] <- factor(value, levels = levels(train[[j]]))
    }
  }
  test
}

# Median imputation of numeric features with train medians; factor NA -> own level.
impute <- function(train, test) {
  for (j in seq_along(train)) {
    if (is.numeric(train[[j]])) {
      m <- stats::median(train[[j]], na.rm = TRUE)
      train[[j]][is.na(train[[j]])] <- m
      test[[j]][is.na(test[[j]])] <- m
    } else if (is.factor(train[[j]])) {
      train[[j]] <- factor(ifelse(is.na(train[[j]]), "missing", as.character(train[[j]])))
      test[[j]] <- factor(ifelse(is.na(test[[j]]), "missing", as.character(test[[j]])))
    }
  }
  list(train = droplevels(train), test = align_factors(droplevels(train), test))
}

# Controls shared by the tree-based methods: leaves of at least 5% of the training data, depth 4.
min_bucket <- function(train) max(20L, floor(0.05 * nrow(train)))

constant_columns <- function(train) {
  !vapply(train, function(column) length(unique(column)) > 1L, logical(1L))
}

# ---- competitors ------------------------------------------------------------------------

method_logit <- function(train, test, y_train, ...) {
  train <- train[, !constant_columns(train), drop = FALSE]
  train$y <- y_train
  model <- suppressWarnings(stats::glm(y ~ ., family = stats::binomial, data = train))
  p <- suppressWarnings(stats::predict(model, newdata = test, type = "response"))
  list(p = as.numeric(p), npar = sum(!is.na(stats::coef(model))), segments = 1L)
}

method_ctree <- function(train, test, y_train, ...) {
  train$y <- factor(y_train)
  tree <- partykit::ctree(y ~ ., data = train, control = partykit::ctree_control(
    alpha = 0.1, minbucket = min_bucket(train), maxdepth = 4
  ))
  list(p = as.numeric(stats::predict(tree, newdata = test, type = "prob")[, "1"]), npar = NA_real_, segments = width(tree))
}

method_mob <- function(train, test, y_train, ...) {
  train <- train[, !constant_columns(train), drop = FALSE]
  train$y <- y_train
  tree <- suppressWarnings(partykit::glmtree(
    y ~ . | ., data = train, family = stats::binomial, minsize = min_bucket(train), maxdepth = 4
  ))
  list(p = as.numeric(suppressWarnings(stats::predict(tree, newdata = test, type = "response"))), npar = NA_real_, segments = width(tree))
}

method_forest <- function(train, test, y_train, ...) {
  train$y <- factor(y_train)
  forest <- ranger::ranger(y ~ ., data = train, probability = TRUE, num.trees = 500, num.threads = 1L)
  list(p = as.numeric(stats::predict(forest, data = test)$predictions[, "1"]), npar = NA_real_, segments = NA_integer_)
}

glmtree_controls <- function(train) {
  partykit::ctree_control(alpha = 0.1, minbucket = min_bucket(train), maxdepth = 4)
}

method_glmtree <- function(train, test, y_train, ...) {
  fit <- glmtree::glmtree(train, y_train,
    K = 5, iterations = sem_iter, criterion = "bic", ctree_controls = glmtree_controls(train)
  )
  list(p = as.numeric(predict(fit, test)), npar = NA_real_, segments = length(fit@best.tree$labels),
       best_iteration = fit@performance$best_iteration)
}

method_glmtree_validation <- function(train, test, y_train, ...) {
  fit <- glmtree::glmtree(train, y_train,
    K = 5, iterations = sem_iter, validation = TRUE, proportions = 0.25, criterion = "gini",
    ctree_controls = glmtree_controls(train)
  )
  list(p = as.numeric(predict(fit, test)), npar = NA_real_, segments = length(fit@best.tree$labels),
       best_iteration = fit@performance$best_iteration)
}

# ---- one repetition ---------------------------------------------------------------------

run_repetition <- function(dataset_name, dataset, rep, methods) {
  if (!is.null(dataset$generate)) dataset <- c(dataset, dataset$generate(rep))
  set.seed(1000L + rep)
  X <- clean_types(dataset$X)
  y <- dataset$y
  train_index <- sort(unlist(lapply(split(seq_along(y), y), function(idx) sample(idx, floor(0.7 * length(idx))))))
  imputed <- impute(X[train_index, , drop = FALSE], X[-train_index, , drop = FALSE])
  train <- imputed$train
  test <- imputed$test
  rows <- list()
  for (name in names(methods)) {
    set.seed(5000L + rep)
    elapsed <- system.time(result <- tryCatch(methods[[name]](train, test, y[train_index]), error = function(e) e))[["elapsed"]]
    row <- data.frame(dataset = dataset_name, rep = rep, method = name, gini = NA_real_, segments = NA_real_,
                      best_iteration = NA_real_, secs = elapsed, note = "")
    if (inherits(result, "error")) {
      row$note <- conditionMessage(result)
    } else {
      ok <- !is.na(result$p)
      row$gini <- gini(y[-train_index][ok], result$p[ok])
      row$segments <- result$segments
      if (!is.null(result$best_iteration)) row$best_iteration <- result$best_iteration
    }
    rows[[length(rows) + 1L]] <- row
  }
  do.call(rbind, rows)
}
