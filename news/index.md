# Changelog

## glmtree 0.4.0

CRAN release: 2026-10-11

This release re-submits the package to CRAN, from which it was archived
on 2025-10-03: the vignette called
[`library(FactoMineR)`](http://factominer.free.fr) unconditionally while
`FactoMineR` is only suggested, and the build failed on a check flavor
where it was not installed. The vignette is rewritten and the optional
chunk is only evaluated when `FactoMineR` is available.

### Bug fixes

- **`test` and `validation` now do what the documentation says.** Until
  now the held out sets were drawn but never used: all regressions,
  trees and criteria were computed on all the observations. The training
  set is now the only data used to fit; `validation` selects the
  iteration; `test` is used only to report the performance of the best
  tree (`performance$performance`).
- The validation versions of the criteria were broken in version 0.3.1:
  `criterion = "aic"` with `validation = TRUE` failed with “factor x1
  has new levels”, the BIC returned `-Inf`, and the Gini mixed the
  labels of all validation observations with the predictions of one
  segment. All are now computed correctly (log-likelihood or Gini on the
  held out set).
- The BIC penalty of a segment used `log(n)` of the whole data set
  because of an operator precedence error; it now uses the size of the
  segment, as intended.
- With fewer than 50 iterations the “best” tree was always the one of
  iteration 2, drawn from a random segmentation, and `iterations = 1`
  failed with “object ‘best_link’ not found”. The burn-in is now the
  argument `burn_in` (default: the minimum of 50 and a quarter of
  `iterations`) and a tree is always returned.
- Integer columns, tibbles, matrices, character and logical columns are
  accepted in `x`; logical and factor responses are accepted for `y`.
  Missing values are reported with an error.
- [`normalizedGini()`](https://adimajo.github.io/glmtree/reference/normalizedGini.md)
  is computed from ranks (2 AUC - 1): it no longer depends on the order
  of the observations when predictions are tied, and returns `NA` if
  there is a single class.
- The class documentation was filed under `glmdisc-class`; it is now
  `glmtree-class`.

### New features

- [`predict()`](https://adimajo.github.io/glmtree/reference/predict.md)
  for `glmtree` objects (probabilities, or `type = "segment"`), robust
  to factor levels unseen in training (they get the rate of positives of
  their segment); `show()` /
  [`print()`](https://rdrr.io/r/base/print.html).
- New arguments `burn_in`, `patience` (stop the chain when the criterion
  stopped improving) and `verbose` (the criterion was printed at every
  iteration; it is now silent by default).
- `best.tree` also stores `coefficients`, `labels`, `rates` etc. used to
  predict; the `glm` objects of `best.tree$glms` are named by segment.
- `performance$best_iteration` and `parameters$sizes` (training /
  validation / test sizes).

### Changes

- The SEM loop was rewritten: one `glm.fit` per segment on a design
  matrix built once instead of
  [`glm()`](https://rdrr.io/r/stats/glm.html) +
  [`predict()`](https://adimajo.github.io/glmtree/reference/predict.md)
  on the whole data frame for each segment and iteration, a single
  prediction of the tree per iteration instead of two, vectorized
  sampling of the segments instead of `apply(sample())` over the rows.
  See `inst/benchmarks/speed.R` for the timings.
- Dependencies reduced to `partykit` (`dplyr`, `magrittr` and `caret`
  are no longer needed); `FactoMineR` is only used in an optional
  vignette chunk.
- Defaults as documented: `test = FALSE`, `validation = FALSE`,
  `criterion = "bic"`.
- [`generateData()`](https://adimajo.github.io/glmtree/reference/generateData.md)
  is vectorized (same distributions, different random stream).
- CI: standard `r-lib/actions` workflows (check on Linux, macOS and
  Windows plus a job without the suggested packages, coverage, styler,
  pkgdown) replace the previous single workflow; `renv`, Travis and the
  committed `docs/` were removed.
- Added `inst/benchmarks/`, a comparison with a logistic regression,
  `ctree`, model-based recursive partitioning and a random forest on
  simulated and real data.
