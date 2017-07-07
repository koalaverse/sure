# Load required packages
library(ordr)

context("Utility functions")

if (require(MASS, quietly = TRUE)) {
  data(housing, package = "MASS")
  fit.polr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  test_that("ncat works for \"polr\" objects", {
    expect_equal(ncat(fit.polr), 3)
  })
}
