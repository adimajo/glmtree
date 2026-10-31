# glmtree

R package from a PhD thesis: logistic regression trees (a tree whose leaves hold logistic
regressions) fitted by a stochastic EM (SEM) algorithm. Entry point: `glmtree()` in
`R/glmtree.R`. S4 class `glmtree`. Sister package: `../glmdisc` (same conventions).

## Layout

| File | Role |
|---|---|
| `R/glmtree.R` | `glmtree()`: checks, split, the SEM loop, best-iteration selection, final models, S4 object |
| `R/helpers.R` | internal helpers: feature checks / alignment, design matrix, `glm.fit` per segment, criteria, vectorized draw, `predict_core()` |
| `R/predict.R` | S4 methods `predict`, `show`, `print` |
| `R/cutDataset.R` | train / validation / test split |
| `R/generateData.R`, `R/normalizedGini.R`, `R/initialization.R` | simulation, rank-based Gini (2 AUC - 1), random start |
| `inst/benchmarks/` | comparison on real datasets, convergence study, old-vs-new speed (not run on CRAN) |

Key design points (read before changing the SEM loop):
- Segments are **integer labels** kept for the whole chain (never recoded / refactored). `c_hat` is
  the sampled segment, `c_map` the tree's most probable segment; the criterion of iteration `i` is
  computed on `c_map` with the tree of iteration `i - 1` (the one that produced it), and that pair
  is what is stored as "best".
- Logistic regressions are `glm.fit` on a design matrix built once; aliased coefficients are set to 0;
  prediction is a coefficient lookup (`best.tree$coefficients`). The `glm` objects in
  `best.tree$glms` are for inspection and are fitted on the same design columns.
- Held-out sets: `validation` is used to select the iteration, `test` only to report the performance
  of the best tree. Unseen factor levels at prediction time get the positive rate of their segment.
- The S4 class name `glmtree` clashes with the S3 class of `partykit::glmtree()` (MOB) objects:
  the `predict` / `print` methods must hand non-S4 objects to `callNextMethod()` and the package
  must NOT register an S3 `predict.glmtree` (it would override partykit's). Tested.
- BIC of a segment uses `log(segment size)`; kept from the original code on purpose, flag it before changing.
- `ctree` is ~70% of the runtime; speed ups on the rest are done (see NEWS / benchmarks).

## Commands

R 4.6.x from the CRAN apt repo; R packages live in `~/R/library` (Posit binaries):
always set `R_LIBS_USER=$HOME/R/library`. Run R from outside the repo only if a project `.Rprofile` reappears.

```bash
# regenerate NAMESPACE / Rd (roxygen without compiling)
Rscript -e 'roxygen2::roxygenise(".", load_code = roxygen2::load_source)'
# install, test, check
R CMD INSTALL --no-docs -l /tmp/glmtree-lib . && Rscript -e '.libPaths(c("/tmp/glmtree-lib", .libPaths())); testthat::test_local(".")'
R CMD build . && R CMD check --as-cran glmtree_*.tar.gz
# reproduce the CRAN incoming checks, including the spell check of DESCRIPTION
# (needs aspell and aspell-en; the variable is set on the CRAN machines, not by --as-cran)
_R_CHECK_CRAN_INCOMING_USE_ASPELL_=true R CMD check --as-cran glmtree_*.tar.gz
# spelling of Rd, README, NEWS and vignette; unknown words go to inst/WORDLIST
Rscript -e 'spelling::spell_check_package(".")'
# URLs (a redirect, e.g. a missing trailing slash, is reported by CRAN)
Rscript -e 'urlchecker::url_check()'
# style (CI fails on a diff)
Rscript -e 'styler::style_pkg()'
# benchmarks
Rscript inst/benchmarks/real_data.R 20 100 inst/benchmarks/results
Rscript inst/benchmarks/convergence.R inst/benchmarks/results
```

Pitfalls:
- Tests must be seeded (`set.seed()`); the chain is stochastic.
- `glm()` drops unused factor levels (and so changes the reference level): never use its coefficient
  names to predict, use the design matrix of `R/helpers.R`.
- Background `Rscript` via a `( ... ) &` subshell died silently here: run long jobs with the tool's own background mode.

## Conventions

- tidyverse style via `styler` (CI checks `styler::style_pkg(dry = "fail")`), roxygen2 (markdown on),
  testthat 3rd edition. Imports: partykit, stats, graphics, methods (no new ones without discussion).
- No claims in docs / README without a number from `inst/benchmarks/`.
- Maintainer address: adrien.ehrhardt@polytechnique.edu (the package was archived from CRAN on
  2025-10-03 because a vignette needed `FactoMineR`, only in Suggests, on one check flavor).
