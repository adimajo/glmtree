<!-- badges: start -->
[![R-CMD-check](https://github.com/adimajo/glmtree/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/adimajo/glmtree/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/adimajo/glmtree/branch/master/graph/badge.svg)](https://app.codecov.io/gh/adimajo/glmtree?branch=master)
<!-- badges: end -->

# glmtree: logistic regression trees

`glmtree` builds decision trees with a logistic regression at each leaf: the tree cuts the
population into segments, and each segment gets its own, interpretable, scoring function. The
segmentation is searched jointly with the regressions by a stochastic EM algorithm, as described in
*Formalization and study of statistical problems in Credit Scoring*, Ehrhardt A. (see the
[manuscript](https://github.com/adimajo/manuscrit_these) or the
[web article](https://adimajo.github.io/logistic_trees.html)).

> **Status.** The package was archived on CRAN on 2025-10-03 (a vignette needed a suggested package
> that was missing on one check flavor) and a re-submission is in preparation. Meanwhile, install it
> from GitHub.

## Installation

``` r
# install.packages("pak")
pak::pak("adimajo/glmtree")
```

## Example

``` r
library(glmtree)

set.seed(1)
data <- generateData(n = 2000, scenario = "tree")   # 3 segments, 3 different regressions
features <- c("x1", "x2", "x3")

fit <- glmtree(data[, features], data$y, K = 5, iterations = 100,
               ctree_controls = partykit::ctree_control(alpha = 0.1, minbucket = 100, maxdepth = 4))
fit                                                  # number of segments, criterion, the tree
predict(fit, data[1:5, features])                    # probabilities
predict(fit, data[1:5, features], type = "segment")  # segments
fit@best.tree$glms                                   # the logistic regression of each segment
```

`partykit` also exports a function called `glmtree()` (model-based recursive partitioning). If you
attach both packages, call ours as `glmtree::glmtree()`. See `vignette("glmtree")` for the interface
(`validation`, `test`, `criterion`, `burn_in`, `patience`, ...).

## What to expect: measured results

The numbers below come from `inst/benchmarks/real_data.R` (20 random stratified 70/30 splits per
dataset, 100 SEM iterations, `K = 5`; the full table and the raw results are in
`inst/benchmarks/results/`). They are test-set **Gini** coefficients (2 AUC - 1, higher is better), means
over the splits. Tree-based methods use leaves of at least 5% of the training data and depth 4.
`glmtree` selects the tree with the BIC on the training data, `glmtree (valid.)` uses 25% of the
training data as a validation set and the Gini. MOB is `partykit::glmtree()`, the
model-based recursive partitioning, with all features both as regressors and as partitioning variables.

| Dataset | logit | `ctree` | MOB | random forest | `glmtree` | `glmtree` (valid.) |
|---|---|---|---|---|---|---|
| Simulated, 3 segments (n = 3000) | 0.248 | 0.568 | **0.698** | 0.657 | 0.650 | 0.644 |
| Simulated, 1 regression (n = 3000) | **0.672** | 0.619 | **0.672** | 0.606 | **0.672** | 0.670 |
| German credit (1000 × 20) | 0.552 | 0.439 | 0.436 | **0.579** | 0.514 | 0.393 |
| Pima diabetes (532 × 7) | **0.704** | 0.591 | **0.704** | 0.665 | 0.697 | 0.633 |
| ISLR `Default` (10000 × 3) | **0.896** | 0.867 | **0.896** | 0.849 | 0.894 | 0.893 |
| AER `CreditCard` (1319 × 10) | 0.670 | 0.608 | 0.654 | **0.692** | 0.667 | 0.668 |
| ISLR `Caravan` (5822 × 85) | **0.439** | 0.400 | **0.439** | 0.426 | 0.413 | 0.379 |

What this says, honestly:

* **`glmtree` is not a general-purpose accuracy booster.** It helps when the population really is made
  of segments with different regressions: on the simulation with three segments it goes from 0.248 (one
  logistic regression) to 0.650, using 4.1 segments on average (three are true). On the five real datasets
  no single-tree method (`ctree`, MOB, `glmtree`) beats a plain logistic regression: `glmtree` is between
  0.002 and 0.038 below it, because the data are close to a single regression and the chain mostly keeps
  one or two segments (1.2 to 1.7 on average). Only the random forest is ahead, on German credit
  (+0.027 ± 0.007, paired mean ± standard error) and `CreditCard` (+0.022 ± 0.006).
* **`partykit::glmtree()` (MOB) is the stronger and faster alternative on these data.** On the
  simulation with segments MOB scores higher than `glmtree` (0.698 against 0.650) and takes 1.4 s per fit
  against 11 s. On the real data sets MOB either does not split (it is then identical to the logit: Pima,
  `Default`, `Caravan`) or loses (German credit 0.436, `CreditCard` 0.654), where `glmtree` stays close
  to the logit. `glmtree` is neither faster nor more accurate than MOB in this benchmark; what it offers
  is a different, likelihood-based search (a latent segment variable), a choice of criterion (BIC, AIC,
  held-out Gini) and `ctree` for the segmentation.
* **The validation set version is the worse choice on small samples** (German credit 0.393, Pima 0.633):
  holding out data makes both the fit and the selection noisy. Prefer the BIC on the training data
  unless the sample is large.
* **Cost.** A fit of 100 iterations takes 3 to 18 s on the data sets above (5 s for 1319 observations,
  18 s for 7000 training observations; 160 s for the 85 features of `Caravan`), measured with six fits running
  in parallel. About 70% of the time is spent in `partykit::ctree`, which is refitted at every iteration.
  Compared with this package's own version 0.3.1, the rewrite of version 0.4.0 made an iteration about
  twice as fast for the same accuracy (2.4×, 2.2× and 2.0× for 1000, 3000 and 10000 observations, 50 iterations;
  `inst/benchmarks/speed.R`).
* **Convergence is slow in iterations.** On the simulation with segments, the test Gini rises with the
  iteration budget (BIC on the training data, mean over 12 repetitions): 0.639 (25 iterations), 0.645 (50),
  0.652 (100), 0.658 (200), 0.664 (400), 0.676 (800), and the best iteration is still far into the chain
  (349 on average for 800). With `patience = 25` the chain stops after about 51 iterations in about 5 s at
  a Gini of 0.646, against 75 s for 800 iterations: use `patience` for a quick answer, and a large
  `iterations` if the extra 0.03 Gini matters (`inst/benchmarks/convergence.R`).

Reproduce with `Rscript inst/benchmarks/real_data.R 20 100` and `Rscript inst/benchmarks/convergence.R`
(they need `partykit`, `ranger`, `scorecard`, `ISLR`, `AER`; about an hour).

## Documentation

* Help pages and vignette: `?glmtree`, `vignette("glmtree")`.
* Online: <https://adimajo.github.io/glmtree>.
* Questions and bug reports: <https://github.com/adimajo/glmtree/issues>.

## References

* Celeux, G., Chauveau, D., Diebolt, J. (1995), On Stochastic Versions of the EM Algorithm.
  Research Report RR-2514, INRIA.
* Hothorn, T., Zeileis, A. (2015), partykit: A Modular Toolkit for Recursive Partytioning in R.
  Journal of Machine Learning Research 16, 3905-3909.
