# Align new features with the training features

Factor levels not seen in training are set to `NA` (and flagged) so that
trees and design matrices can be applied to them.

## Usage

``` r
align_features(x, features)
```

## Arguments

- x:

  New features.

- features:

  Output of
  [`feature_description()`](https://adimajo.github.io/glmtree/reference/feature_description.md).

## Value

The aligned data frame and a logical vector flagging the rows with
unseen levels.
