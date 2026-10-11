# Split the observations into training, validation and test sets

Split the observations into training, validation and test sets

## Usage

``` r
cutDataset(n, proportions, test = TRUE, validation = TRUE)
```

## Arguments

- n:

  The number of observations.

- proportions:

  Proportions of the test set and of the validation set:
  `c(test, validation)` when both are requested, a single proportion for
  whichever of the two is requested.

- test:

  Whether to build a test set.

- validation:

  Whether to build a validation set.

## Value

A list with the integer row indices `train`, `validation` and `test`
(`NULL` when not requested).

## Author

Adrien Ehrhardt
