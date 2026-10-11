# Criterion (higher is better) of a segmentation on the data it was fitted on

AIC and BIC are penalized per segment: the BIC of a segment uses the
logarithm of its size.

## Usage

``` r
training_criterion(criterion, y, p, seg, rank)
```
