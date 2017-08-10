context("Utility functions")


test_that("utility functions work for \"clm\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("ordinal")

  # Load data
  data(df1)

  # Fit cumulative link model
  fit <- ordinal::clm(y ~ x + I(x ^ 2), data = df1, link = "probit")

  # Expectations
  expect_equal(length(getBounds(fit)), 5)
  expect_equal(getDistributionFunction(fit), pnorm)
  expect_equal(getDistributionName(fit), "norm")
  expect_equal(getQuantileFunction(fit), qnorm)
  expect_identical(getResponseValues(fit), as.integer(df1$y))
  expect_equal(ncat(fit), 4)

})


test_that("utility functions work for \"glm\" objects", {

  # Skips
  skip_on_cran()

  # Load data
  data(df1)

  # Fit binary probit model
  suppressWarnings(
    fit <- stats::glm(y ~ x + I(x ^ 2), data = df1,
                      family = binomial(link = "probit"))
  )

  # Expectations
  expect_null(getBounds(fit))
  expect_equal(getDistributionFunction(fit), pnorm)
  expect_equal(getDistributionName(fit), "norm")
  expect_equal(getQuantileFunction(fit), qnorm)
  expect_identical(getResponseValues(fit), as.integer(df1$y) - 1)
  # expect_equal(ncat(fit), 4)

})


test_that("utility functions work for \"lrm\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("rms")

  # Load data
  data(df1)

  # Fit cumulative link model
  fit <- rms::lrm(y ~ x, data = df1)

  # Expectations
  expect_equal(length(getBounds(fit)), 5)
  expect_equal(getDistributionFunction(fit), plogis)
  expect_equal(getDistributionName(fit), "logis")
  expect_equal(getQuantileFunction(fit), qlogis)
  expect_identical(getResponseValues(fit), as.integer(df1$y))
  expect_equal(ncat(fit), 4)

})


test_that("utility functions work for \"lrm\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("rms")

  # Load data
  data(df1)

  # Fit cumulative link model
  fit <- rms::orm(y ~ x, data = df1, family = probit)

  # Expectations
  expect_equal(length(getBounds(fit)), 5)
  expect_equal(getDistributionFunction(fit), pnorm)
  expect_equal(getDistributionName(fit), "norm")
  expect_equal(getQuantileFunction(fit), qnorm)
  expect_identical(getResponseValues(fit), as.integer(df1$y))
  expect_equal(ncat(fit), 4)

})


test_that("utility functions work for \"polr\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("MASS")

  # Load data
  data(df1)

  # Fit cumulative link model
  fit <- MASS::polr(y ~ x + I(x ^ 2), data = df1, method = "probit")

  # Expectations
  expect_equal(length(getBounds(fit)), 5)
  expect_equal(getDistributionFunction(fit), pnorm)
  expect_equal(getDistributionName(fit), "norm")
  expect_equal(getQuantileFunction(fit), qnorm)
  expect_identical(getResponseValues(fit), as.integer(df1$y))
  expect_equal(ncat(fit), 4)

})


test_that("utility functions work for \"vglm\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("VGAM")

  # Load data
  data(df1)

  # Fit cumulative link model
  suppressWarnings(
    fit <- VGAM::vglm(y ~ x + I(x ^ 2), data = df1,
                      family = VGAM::cumulative(link = probit, parallel = TRUE))
  )

  # Expectations
  expect_equal(length(getBounds(fit)), 5)
  expect_equal(getDistributionFunction(fit), pnorm)
  expect_equal(getDistributionName(fit), "norm")
  expect_equal(getQuantileFunction(fit), qnorm)
  expect_identical(getResponseValues(fit), as.integer(df1$y))
  expect_equal(ncat(fit), 4)

})


test_that("getMeanResponse works", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("ordinal")
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  skip_if_not_installed("VGAM")

  # Load data
  data(df1)

})
