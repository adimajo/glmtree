# Draw one segment per row with probability proportional to `weights`

Gumbel-max trick: vectorized categorical sampling.

## Usage

``` r
draw_segments(weights, labels)
```

## Arguments

- weights:

  Matrix of non-negative weights, one column per segment.

- labels:

  Segment of each column.
