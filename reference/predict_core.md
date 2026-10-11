# Predictions of a fitted logistic regression tree on new features

Predictions of a fitted logistic regression tree on new features

## Usage

``` r
predict_core(model, newdata)
```

## Arguments

- model:

  The `best.tree` slot of a `glmtree` object.

- newdata:

  New features.

## Value

A list with the predicted probabilities and the segments.
