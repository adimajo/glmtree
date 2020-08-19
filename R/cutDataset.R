cutDataset <- function(n,
                       proportions,
                       test = TRUE,
                       validation = TRUE) {
  if (test == TRUE) {
    if (validation == TRUE) {
      if (tryCatch(
        length(proportions) < 2 |
          proportions[1] <= 0 |
          proportions[2] <= 0 |
          sum(proportions) >= 1,
        error = function() {
          stop(
            simpleError(
              "Argument proportions should contain 2 positive arguments which sum should be less than 1"
            )
          )
        }
      )) {
        stop(
          simpleError(
            "Argument proportions should contain 2 positive arguments which sum should be less than 1"
          )
        )
      }
      ind_train <- sample.int(n, n)
      ind_test <- ind_train[1:floor(proportions[1] * n)]
      ind_validation <- ind_train[(floor(proportions[1] * n) + 1):floor((proportions[1] +
        proportions[2]) * n)]
      ind_train <- ind_train[(floor((proportions[1] + proportions[2]) *
        n) + 1):n]
      return(list(ind_train, ind_test, ind_validation))
    } else {
      if (tryCatch(
        length(proportions) < 1 |
          proportions[1] <= 0 |
          proportions[1] >= 1,
        error = function() {
          stop(
            simpleError(
              "Argument proportions should contain 1 argument strictly between 0 and 1"
            )
          )
        }
      )) {
        stop(
          simpleError(
            "Argument proportions should contain 1 argument strictly between 0 and 1"
          )
        )
      }
      ind_train <- sample.int(n, n)
      ind_test <- ind_train[1:floor(proportions[1] * n)]
      ind_train <- ind_train[(floor(proportions[1] * n) + 1):n]
      return(list(ind_train, ind_test, NULL))
    }
  } else {
    if (validation == TRUE) {
      if (tryCatch(
        length(proportions) < 1 |
          proportions[1] <= 0 |
          proportions[1] >= 1,
        error = function() {
          stop(
            simpleError(
              "Argument proportions should contain 1 argument strictly between 0 and 1"
            )
          )
        }
      )) {
        stop(
          simpleError(
            "Argument proportions should contain 1 argument strictly between 0 and 1"
          )
        )
      }
      ind_train <- sample.int(n, n)
      ind_validation <- ind_train[1:floor(proportions[1] * n)]
      ind_train <- ind_train[(floor(proportions[1] * n) + 1):n]
      return(list(ind_train, ind_validation, NULL))
    } else {
      ind_train <- sample.int(n, n)
      return(list(ind_train, NULL, NULL))
    }
  }
}
