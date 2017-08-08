[![Build Status](https://travis-ci.org/AFIT-R/ordr.svg?branch=master)](https://travis-ci.org/AFIT-R/sure)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/AFIT-R/sure?branch=master&svg=true)](https://ci.appveyor.com/project/AFIT-R/sure)
[![codecov](https://codecov.io/gh/AFIT-R/sure/branch/master/graph/badge.svg)](https://codecov.io/gh/AFIT-R/sure)



# sure: Surrogate Residuals

An R package for constructing surrogate-based [residuals and diagnostics for ordinal regression models](http://amstat.tandfonline.com/doi/abs/10.1080/01621459.2017.1292915).

**Note:** This package also supports surrogate-based residuals for `"glm"` objects with a binomial family using the jittering method described in the above paper.

## Installation

The `sure` package is currently only available from GitHub, but can easilly be installed using [`devtools`](https://CRAN.R-project.org/package=devtools):

```r
# install.packages("devtools")
devtools::install_github("AFIT-R/sure")
```
**Note:** This package is still in development, use at your own risk!
