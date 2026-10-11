# Class glmtree

Class `glmtree` represents a logistic regression tree scheme associated
with its optimal logistic regression models.

## Usage

``` r
# S4 method for class 'glmtree'
show(object)

# S4 method for class 'glmtree'
print(x, ...)
```

## Arguments

- object:

  The S4 `glmtree` object.

- x:

  The S4 `glmtree` object.

- ...:

  Not used.

## Slots

- `parameters`:

  The parameters associated with the method.

- `best.tree`:

  The best tree found by the method given its parameters: `tree` (the
  [`partykit::ctree`](https://rdrr.io/pkg/partykit/man/ctree.html)
  giving the segment of an observation, `NULL` if the chain collapsed to
  a single segment), `glms` (the logistic regression of each segment, a
  named list of `glm` objects) and the elements needed to predict on new
  data.

- `performance`:

  The performance obtained with the method given its parameters:
  `performance` (the criterion of the best iteration, computed on the
  test set if there is one), `criterionEvolution` (the criterion at each
  iteration) and `best_iteration`.
