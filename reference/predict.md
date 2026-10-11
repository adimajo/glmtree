# Prediction of a logistic regression tree

This defines the method "predict" for objects of S4 class
[`glmtree-class`](https://adimajo.github.io/glmtree/reference/glmtree-class.md).

## Usage

``` r
predict(object, ...)

# S4 method for class 'glmtree'
predict(object, newdata, type = c("response", "segment"), ...)
```

## Arguments

- object:

  The S4 `glmtree` object.

- ...:

  Not used.

- newdata:

  A data frame with the features used to fit the tree.

- type:

  `"response"` for the predicted probability of the positive class,
  `"segment"` for the segment of each observation.

## Value

A numeric vector with one prediction per row of `newdata`.

## Details

Applies the best tree to new observations to find their segment, then
the logistic regression of that segment. Observations with a factor
level not seen in training receive the rate of positives of their
segment.

## Examples

``` r
set.seed(1)
data <- generateData(n = 300, scenario = "tree")
fit <- glmtree(
  x = data[, c("x1", "x2", "x3")], y = data$y, K = 5, iterations = 20,
  ctree_controls = partykit::ctree_control(alpha = 0.1, minbucket = 30, maxdepth = 3)
)
table(predict(fit, data[, c("x1", "x2", "x3")], type = "segment"))
#> 
#>   1   2   5 
#>  85  72 143 
```
