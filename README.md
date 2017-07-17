[![Build Status](https://travis-ci.org/AFIT-R/ordr.svg?branch=master)](https://travis-ci.org/AFIT-R/ordr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/AFIT-R/ordr?branch=master&svg=true)](https://ci.appveyor.com/project/AFIT-R/ordr)
[![codecov](https://codecov.io/gh/AFIT-R/ordr/branch/master/graph/badge.svg)](https://codecov.io/gh/AFIT-R/ordr)



# ordr: Ordinal Regression Diagnostics <img src="tools/ordr-logo.png" align="right" width="120" height="139" />

An R package for constructing [residuals and diagnostics for ordinal regression models](http://amstat.tandfonline.com/doi/abs/10.1080/01621459.2017.1292915).


## Installation

The `ordr` package is currently only available from GitHub, but can easilly be installed using [`devtools`](https://CRAN.R-project.org/package=devtools):

```r
# install.packages("devtools")
devtools::install_github("bgreenwell/ordr")
```
*Note:* This package is still in development, use at your own risk!


## Example usage


```r
# Load required packages
library(ggplot2)
library(MASS)
library(ordr)

# Fit a proportional odds model
house.polr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

# Diagnostic plots
grid.arrange(
  autoplot(house.polr, what = "qq", nsim = 50, alpha = 0.5),
  autoplot(house.polr, what = "fitted", nsim = 50, alpha = 0.5),
  autoplot(house.polr, what = "covariate", x = housing$Infl, nsim = 50),
  autoplot(house.polr, what = "covariate", x = housing$Type, nsim = 50),
  ncol = 2
)
#> Warning in data.frame(x = x, y = res): row names were found from a short
#> variable and have been discarded
#> `geom_smooth()` using method = 'gam'
```

![](tools/README-unnamed-chunk-3-1.png)<!-- -->
