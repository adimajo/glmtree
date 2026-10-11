# Calculating the normalized Gini index

This function calculates the Gini index of a classification rule
outputting probabilities. It is a classical metric in the context of
Credit Scoring. It is equal to 2 times the AUC (Area Under ROC Curve)
minus 1, computed from the ranks of the predictions so that the result
does not depend on the order of the observations when predictions are
tied.

## Usage

``` r
normalizedGini(actual, predicted)
```

## Arguments

- actual:

  The numeric binary vector of the actual labels observed.

- predicted:

  The vector of the probabilities predicted by the classification rule.

## Value

The Gini index of the predicted probabilities as a single `num` (`NA` if
`actual` has a single class).

## Author

Adrien Ehrhardt

## Examples

``` r
normalizedGini(c(1, 1, 1, 0, 0), c(0.7, 0.9, 0.5, 0.6, 0.3))
#> [1] 0.6666667
```
