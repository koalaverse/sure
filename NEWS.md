# Changes for version 0.2.2

* Incorporated [pkgdown website](https://koalaverse.github.io/sure/index.html).

* Added paper URLs and ORCIDs to DESCRIPTION file [(#30)](https://github.com/koalaverse/sure/issues/30).

* Added `sure` vignette

* incorporated autoplot methods for glm, lrm, orm, polr, vglm


# Changes for version 0.2.1

* `autoplot()` can now return multiple plots [(#16)](https://github.com/AFIT-R/sure/issues/16).

* Specifying `method = "latent"` now works for binomial GLMs [(27)](https://github.com/koalaverse/sure/issues/27).

* Specifying `method = "jittering"` now issues a warning (at least until it has been fully tested).


### Changes for version 0.2.0

* New function `surrogate` for returning the surrogate response values used in calculating the surrogate-based residuals. The surrogate response values can be useful for checking the proportionality assumption of fitted cumulative link models, among other things.

* Jittering (on both the probability scale and the response scale) is now available for fitted cumulative link models based on packages `MASS`, `ordinal`, `rms`, and `VGAM` [(#18)](https://github.com/AFIT-R/sure/issues/18).

* Added support for vector generalized additive models from the `VGAM` package (i.e., objects of class `"vgam"`).

* New data sets `df4` and `df5` for illustrating various uses of the surrogate residual for diagnostics an ordinal regression models.


# Changes for version 0.1.2

* Initial release.
