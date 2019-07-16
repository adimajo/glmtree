

predict_tweaked <- function(model, df, c_iter) {
  pred = tryCatch(stats::predict(model, newdata = df, type="response"), error = function(e) {
    data_w = df
    data_wo = df
    encodage = caret::dummyVars(~., data=df)
    # Levels not in the training set but in the test set are removed
    for (var in setdiff(encodage$facVars, c("c_map", "c_hat"))) {
      if (length(levels(df[,var])[!(levels(df[,var]) %in% (levels(factor(df[df$c_hat == levels(df$c_hat)[c_iter],var]))))]) > 0) {
        data_w <- data_w[data_w[,var] %in% levels(factor(df[df$c_hat == levels(df$c_hat)[c_iter],var])),]
        data_wo <- data_wo[data_wo[,var] %in% (levels(df[,var])[!(levels(df[,var]) %in% (levels(factor(df[df$c_hat == levels(df$c_hat)[c_iter],var]))))]),]
      }
    }

    pred_w = stats::predict(model, newdata = data_w, type="response")
    pred_wo = rep(mean(df[df$c_hat == c_iter, "y"], na.rm = T), nrow(data_wo))
    pred = c(pred_w, pred_wo)
    names(pred) = c(rownames(data_w), rownames(data_wo))

    return(pred[order(as.numeric(names(pred)))][rownames(df)])
    })
  return(pred)
}
