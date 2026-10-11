# Random initialization of the segmentation

This function randomly initializes the segmentation.

## Usage

``` r
initialization(K, n)
```

## Arguments

- K:

  The number of segments to start with (maximum number of segments
  there'll be in the end).

- n:

  The number of observations to draw.

## Value

An integer vector of length n with values in 1 to K drawn with
replacement.

## Author

Adrien Ehrhardt
