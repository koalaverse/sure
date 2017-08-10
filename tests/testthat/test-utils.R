context("Utility functions")


test_that("utility functions work for \"clm\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("ordinal")

  # Load data
  data(df1)

  # Fit cumulative link model
  fit.logit <- ordinal::clm(y ~ x + I(x ^ 2), data = df1, link = "logit")
  fit.probit <- ordinal::clm(y ~ x + I(x ^ 2), data = df1, link = "probit")
  fit.loglog <- ordinal::clm(y ~ x + I(x ^ 2), data = df1, link = "loglog")
  fit.cloglog <- ordinal::clm(y ~ x + I(x ^ 2), data = df1, link = "cloglog")
  fit.cauchit <- ordinal::clm(y ~ x + I(x ^ 2), data = df1, link = "cauchit")

  # Expectations
  expect_equal(length(getBounds(fit.logit)), 5)
  expect_identical(getResponseValues(fit.logit), as.integer(df1$y))
  expect_equal(ncat(fit.logit), 4)
  expect_equal(getDistributionFunction(fit.logit), plogis)
  expect_equal(getDistributionFunction(fit.probit), pnorm)
  expect_equal(getDistributionFunction(fit.loglog), pgumbel)
  expect_equal(getDistributionFunction(fit.cloglog), pGumbel)
  expect_equal(getDistributionFunction(fit.cauchit), pcauchy)
  expect_equal(getDistributionName(fit.logit), "logis")
  expect_equal(getDistributionName(fit.probit), "norm")
  expect_equal(getDistributionName(fit.loglog), "gumbel")
  expect_equal(getDistributionName(fit.cloglog), "Gumbel")
  expect_equal(getDistributionName(fit.cauchit), "cauchy")
  expect_equal(getQuantileFunction(fit.logit), qlogis)
  expect_equal(getQuantileFunction(fit.probit), qnorm)
  expect_equal(getQuantileFunction(fit.loglog), qgumbel)
  expect_equal(getQuantileFunction(fit.cloglog), qGumbel)
  expect_equal(getQuantileFunction(fit.cauchit), qcauchy)

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


test_that("utility functions work for \"orm\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("rms")

  # Load data
  data(df1)

  # Fit cumulative link model
  fit <- rms::orm(y ~ x, data = df1, family = probit)

  # Fit cumulative link models
  fit.logit <- rms::orm(y ~ x, data = df1, family = logistic)
  fit.probit <- rms::orm(y ~ x, data = df1, family = probit)
  fit.loglog <- rms::orm(y ~ x, data = df1, family = loglog)
  fit.cloglog <- rms::orm(y ~ x, data = df1, family = cloglog)
  fit.cauchit <- rms::orm(y ~ x, data = df1, family = cauchit)

  # Expectations
  expect_equal(length(getBounds(fit.logit)), 5)
  expect_identical(getResponseValues(fit.logit), as.integer(df1$y))
  expect_equal(ncat(fit.logit), 4)
  expect_equal(getDistributionFunction(fit.logit), plogis)
  expect_equal(getDistributionFunction(fit.probit), pnorm)
  expect_equal(getDistributionFunction(fit.loglog), pgumbel)
  expect_equal(getDistributionFunction(fit.cloglog), pGumbel)
  expect_equal(getDistributionFunction(fit.cauchit), pcauchy)
  expect_equal(getDistributionName(fit.logit), "logis")
  expect_equal(getDistributionName(fit.probit), "norm")
  expect_equal(getDistributionName(fit.loglog), "gumbel")
  expect_equal(getDistributionName(fit.cloglog), "Gumbel")
  expect_equal(getDistributionName(fit.cauchit), "cauchy")
  expect_equal(getQuantileFunction(fit.logit), qlogis)
  expect_equal(getQuantileFunction(fit.probit), qnorm)
  expect_equal(getQuantileFunction(fit.loglog), qgumbel)
  expect_equal(getQuantileFunction(fit.cloglog), qGumbel)
  expect_equal(getQuantileFunction(fit.cauchit), qcauchy)

})


test_that("utility functions work for \"polr\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("MASS")

  # Load data
  data(df1)

  # Fit cumulative link models
  fit.logit <- MASS::polr(y ~ x + I(x ^ 2), data = df1, method = "logistic")
  fit.probit <- MASS::polr(y ~ x + I(x ^ 2), data = df1, method = "probit")
  fit.loglog <- MASS::polr(y ~ x + I(x ^ 2), data = df1, method = "loglog")
  fit.cloglog <- MASS::polr(y ~ x + I(x ^ 2), data = df1, method = "cloglog")
  fit.cauchit <- MASS::polr(y ~ x + I(x ^ 2), data = df1, method = "cauchit")

  # Expectations
  expect_equal(length(getBounds(fit.logit)), 5)
  expect_identical(getResponseValues(fit.logit), as.integer(df1$y))
  expect_equal(ncat(fit.logit), 4)
  expect_equal(getDistributionFunction(fit.logit), plogis)
  expect_equal(getDistributionFunction(fit.probit), pnorm)
  expect_equal(getDistributionFunction(fit.loglog), pgumbel)
  expect_equal(getDistributionFunction(fit.cloglog), pGumbel)
  expect_equal(getDistributionFunction(fit.cauchit), pcauchy)
  expect_equal(getDistributionName(fit.logit), "logis")
  expect_equal(getDistributionName(fit.probit), "norm")
  expect_equal(getDistributionName(fit.loglog), "gumbel")
  expect_equal(getDistributionName(fit.cloglog), "Gumbel")
  expect_equal(getDistributionName(fit.cauchit), "cauchy")
  expect_equal(getQuantileFunction(fit.logit), qlogis)
  expect_equal(getQuantileFunction(fit.probit), qnorm)
  expect_equal(getQuantileFunction(fit.loglog), qgumbel)
  expect_equal(getQuantileFunction(fit.cloglog), qGumbel)
  expect_equal(getQuantileFunction(fit.cauchit), qcauchy)

})


test_that("utility functions work for \"vglm\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("VGAM")

  # Load data
  data(df1)

  # Fit cumulative link models
  suppressWarnings(
    fit.logit <- VGAM::vglm(y ~ x + I(x ^ 2), data = df1,
                            family = VGAM::cumulative(link = "logit",
                                                      parallel = TRUE))
  )
  suppressWarnings(
    fit.probit <- VGAM::vglm(y ~ x + I(x ^ 2), data = df1,
                             family = VGAM::cumulative(link = "probit",
                                                       parallel = TRUE))
  )
    # fit.loglog <- VGAM::vglm(y ~ x + I(x ^ 2), data = df1,
    #                          family = VGAM::cumulative(link = "loglog",
    #                                                    parallel = TRUE))
  suppressWarnings(
    fit.cloglog <- VGAM::vglm(y ~ x + I(x ^ 2), data = df1,
                              family = VGAM::cumulative(link = "cloglog",
                                                        parallel = TRUE))
  )
  suppressWarnings(
    fit.cauchit <- VGAM::vglm(y ~ x + I(x ^ 2), data = df1,
                              family = VGAM::cumulative(link = "cauchit",
                                                        parallel = TRUE))
  )

  # Expectations
  expect_equal(length(getBounds(fit.logit)), 5)
  expect_identical(getResponseValues(fit.logit), as.integer(df1$y))
  expect_equal(ncat(fit.logit), 4)
  expect_equal(getDistributionFunction(fit.logit), plogis)
  expect_equal(getDistributionFunction(fit.probit), pnorm)
  # expect_equal(getDistributionFunction(fit.loglog), pgumbel)
  expect_equal(getDistributionFunction(fit.cloglog), pGumbel)
  expect_equal(getDistributionFunction(fit.cauchit), pcauchy)
  expect_equal(getDistributionName(fit.logit), "logis")
  expect_equal(getDistributionName(fit.probit), "norm")
  # expect_equal(getDistributionName(fit.loglog), "gumbel")
  expect_equal(getDistributionName(fit.cloglog), "Gumbel")
  expect_equal(getDistributionName(fit.cauchit), "cauchy")
  expect_equal(getQuantileFunction(fit.logit), qlogis)
  expect_equal(getQuantileFunction(fit.probit), qnorm)
  # expect_equal(getQuantileFunction(fit.loglog), qgumbel)
  expect_equal(getQuantileFunction(fit.cloglog), qGumbel)
  expect_equal(getQuantileFunction(fit.cauchit), qcauchy)

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
