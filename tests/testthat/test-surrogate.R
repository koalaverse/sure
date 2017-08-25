context("Surrogate response values")


test_that("surrogate work for \"polr\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("ordinal")

  # Load data
  data(df1, package = "sure")

  # Fit cumulative link model
  fit <- MASS::polr(y ~ x, data = df1, method = "probit")

  # Compute residuals
  set.seed(101)  # for reproducibility
  s <- surrogate(fit)  # surrogate response values
  set.seed(101)  # for reproducibility
  r <- resids(fit)  # surrogate-based residuals
  mr <- sure:::getMeanResponse.polr(fit)  # mean response

  # Expectations
  expect_equivalent(r, s - mr)

})
