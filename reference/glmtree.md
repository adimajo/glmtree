# Logistic regression tree by Stochastic-Expectation-Maximization

This function produces a logistic regression tree: a decision tree with
logistic regressions at its leaves. The segmentation of the observations
is a latent variable: at each iteration a logistic regression is fitted
in each segment, a conditional inference tree
([`partykit::ctree`](https://rdrr.io/pkg/partykit/man/ctree.html)) is
fitted to the current segmentation, and each observation is re-assigned
to a segment drawn proportionally to the tree probability times the
likelihood of its label under the segment's regression. A few good trees
are kept along the chain and the best one is chosen according to
`criterion`.

## Usage

``` r
glmtree(
  x,
  y,
  K = 10,
  iterations = 200,
  test = FALSE,
  validation = FALSE,
  proportions = c(0.3),
  criterion = "bic",
  ctree_controls = partykit::ctree_control(alpha = 0.1, minbucket = 100, maxdepth = 5),
  burn_in = min(50L, iterations%/%4L),
  patience = NULL,
  verbose = FALSE
)
```

## Arguments

- x:

  The features to use for prediction (data frame or matrix with numeric,
  integer and factor columns; characters and logicals are converted to
  factors). No missing values are allowed.

- y:

  The binary / boolean labels to predict. The larger value (or the
  second level of a factor) is the positive class.

- K:

  The number of segments to start with (maximum number of segments
  there'll be in the end).

- iterations:

  The number of iterations to do in the SEM protocole (default: 200).

- test:

  Boolean : True if a test set should be held out, on which the
  `criterion` of the best tree is reported in `performance` (default:
  FALSE).

- validation:

  Boolean : True if a validation set should be held out, on which the
  `criterion` is computed at each iteration to choose the best tree
  (default: FALSE, the criterion is then computed on the training set).

- proportions:

  The proportions of observations held out: `c(test, validation)` when
  both are requested, otherwise a single proportion for whichever is
  requested. Not used when both are FALSE. Errors when invalid (e.g. a
  sum greater than one). Default: 0.3.

- criterion:

  The criterion ('gini','aic','bic') used to choose the best tree among
  the generated ones (default: 'bic'). AIC and BIC are summed over
  segments (the BIC penalty of a segment uses the logarithm of its size)
  and are replaced by the log-likelihood when computed on held out data,
  where no penalty is needed. The Gini index is best used with a
  validation set.

- ctree_controls:

  The controls to use for
  [`partykit::ctree`](https://rdrr.io/pkg/partykit/man/ctree.html).

- burn_in:

  The number of first iterations that cannot be chosen as the best tree
  (default: the minimum of 50 and a quarter of `iterations`).

- patience:

  If not `NULL`, the chain stops when the criterion did not improve for
  `patience` iterations (after the burn-in).

- verbose:

  Whether to print the criterion at each iteration.

## Value

An S4 object of class
[`glmtree-class`](https://adimajo.github.io/glmtree/reference/glmtree-class.md)
that contains the parameters used to search for the logistic regression
tree, the best tree it found, and its performance. Use
[`predict`](https://adimajo.github.io/glmtree/reference/predict.md) to
predict on new data.

## Author

Adrien Ehrhardt

## Examples

``` r
set.seed(1)
data <- generateData(n = 300, scenario = "tree")
fit <- glmtree(
  x = data[, c("x1", "x2", "x3")], y = data$y, K = 5, iterations = 20,
  ctree_controls = partykit::ctree_control(alpha = 0.1, minbucket = 30, maxdepth = 3)
)
fit
#> Error in x$info: $ operator not defined for this S4 class
head(predict(fit, data[, c("x1", "x2", "x3")]))
#> [1] 0.6456856 0.6788915 0.6827978 0.7914840 0.4498238 0.8821124
```
