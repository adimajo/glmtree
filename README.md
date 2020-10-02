![CRAN package](https://www.r-pkg.org/badges/version-ago/glmtree)
![CRAN downloads](https://cranlogs.r-pkg.org/badges/glmtree)
![R package](https://github.com/adimajo/glmtree/workflows/R%20package/badge.svg)
[![Travis build status](https://travis-ci.org/adimajo/glmtree.svg?branch=master)](https://travis-ci.org/adimajo/glmtree)
[![Coverage status](https://codecov.io/gh/adimajo/glmtree/branch/master/graph/badge.svg)](https://codecov.io/github/adimajo/glmtree?branch=master)

# Logistic regression trees

Table of Contents
-----------------

* [Documentation](https://adimajo.github.io/glmtree)
* [Installation instructions](#-installation)
* [Open an issue](https://github.com/adimajo/glmtree/issues/new/choose)

# glmtree

The goal of glmtree is to build decision trees with logistic regressions at their leaves, so that the resulting model mixes non parametric VS parametric and stepwise VS linear approaches to have the best predictive results, yet maintaining interpretability.

This is the implementation of glmtree as described in *Formalization and study of statistical problems in Credit Scoring*, Ehrhardt A. (see [manuscript](https://github.com/adimajo/manuscrit_these) or [web article](https://adimajo.github.io/logistic_trees.html))

## Installation

You can install the development version of `glmtree` from Github with:

``` r
# install.packages("devtools")
devtools::install_github("adimajo/glmtree", build_vignettes = TRUE)
```

Or alternatively directly from CRAN:
``` r
install.packages("glmtree")
```

## Documentation

### Through R(Studio)

#### Getting help

The help pages of this package can be reached by typing `help(glmtree)` (or `?glmtree`) once the package is installed and loaded.

#### Vignette

For further instructions, go to `vignettes` > `glmtree.Rmd` or go through the vignette directly in RStudio by typing `vignette('glmtree')` once the package is installed and loaded.

### Online

Both the help pages and the vignette are available online as [a Github page built with `pkgdown`](http://adimajo.github.io/glmtree).
