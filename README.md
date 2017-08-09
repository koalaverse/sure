[![Build Status](https://travis-ci.org/AFIT-R/sure.svg?branch=master)](https://travis-ci.org/AFIT-R/sure)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/AFIT-R/sure?branch=master&svg=true)](https://ci.appveyor.com/project/AFIT-R/sure)
[![codecov](https://codecov.io/gh/AFIT-R/sure/branch/master/graph/badge.svg)](https://codecov.io/gh/AFIT-R/sure)



# sure: Surrogate Residuals

An R package for constructing **SU**rrogate-based **RE**siduals and diagnostics for ordinal and general regression models; based on the approach described in [Dungang and Zhang (2017)](http://www.tandfonline.com/doi/abs/10.1080/01621459.2017.1292915?journalCode=uasa20).


## Installation

The `sure` package is currently only available from GitHub, but can easilly be installed using [`devtools`](https://CRAN.R-project.org/package=devtools):

```r
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("AFIT-R/sure")
```
**Note:** This package is still in development, use at your own risk!


## References

Liu, Dungang and Zhang, Heping. Residuals and Diagnostics for Ordinal Regression Models: A Surrogate Approach.
*Journal of the American Statistical Association* (accepted). URL
http://www.tandfonline.com/doi/abs/10.1080/01621459.2017.1292915?journalCode=uasa20
