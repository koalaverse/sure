# sure 0.2.2.9000

## User-visible changes

* Jittering on the probability scale seems broken and is currently only available for logit-type models.

## Enhancements

* Incorporated [pkgdown website](https://koalaverse.github.io/sure/index.html).

* Added paper URLs and ORCIDs to DESCRIPTION file [(#30)](https://github.com/koalaverse/sure/issues/30).

* Added `sure` vignette

* incorporated autoplot methods for glm, lrm, orm, polr, vglm

* `autoplot()` can now return multiple plots [(#16)](https://github.com/koalaverse/sure/issues/16).

* Specifying `method = "latent"` now works for binomial GLMs [(27)](https://github.com/koalaverse/sure/issues/27).

* Specifying `method = "jittering"` now issues a warning (at least until it has been fully tested).


# sure 0.2.0

* New function `surrogate` for returning the surrogate response values used in calculating the surrogate-based residuals. The surrogate response values can be useful for checking the proportionality assumption of fitted cumulative link models, among other things.

* Jittering (on both the probability scale and the response scale) is now available for fitted cumulative link models based on packages `MASS`, `ordinal`, `rms`, and `VGAM` [(#18)](https://github.com/koalaverse/sure/issues/18).

* Added support for vector generalized additive models from the `VGAM` package (i.e., objects of class `"vgam"`).

* New data sets `df4` and `df5` for illustrating various uses of the surrogate residual for diagnostics an ordinal regression models.


# sure 0.1.2

* Initial release.
