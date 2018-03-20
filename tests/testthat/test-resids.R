context("Surrogate residuals")


test_that("resids work for \"clm\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("ordinal")

  # Load data
  data(df1)

  # Fit cumulative link model
  fit <- ordinal::clm(y ~ x + I(x ^ 2), data = df1, link = "logit")

  # Compute residuals
  res1 <- resids(fit)
  res2 <- resids(fit, nsim = 10)

  # Expectations
  expect_equal(length(res1), nrow(df1))
  expect_equal(length(res2), nrow(df1))
  expect_null(attr(res1, "boot_reps"))
  expect_null(attr(res1, "boot_id"))
  expect_is(attr(res2, "boot_reps"), "matrix")
  expect_is(attr(res2, "boot_id"), "matrix")
  expect_equal(dim(attr(res2, "boot_reps")), c(nrow(df1), 10))
  expect_equal(dim(attr(res2, "boot_id")), c(nrow(df1), 10))

})


test_that("resids work for \"glm\" objects", {

  # Skips
  skip_on_cran()

  # Load data
  data(df1)

  # Fit cumulative link model
  fit <- glm(y ~ x + I(x ^ 2), data = df1, family = binomial)

  # Compute residuals
  res1 <- resids(fit)
  res2 <- resids(fit, nsim = 10)
  res3 <- resids(fit, jitter.scale = "probability")
  res4 <- resids(fit, nsim = 10, jitter.scale = "probability")

  # Expectations
  expect_equal(length(res1), nrow(df1))
  expect_equal(length(res2), nrow(df1))
  expect_equal(length(res3), nrow(df1))
  expect_equal(length(res4), nrow(df1))
  expect_null(attr(res1, "boot_reps"))
  expect_null(attr(res1, "boot_id"))
  expect_null(attr(res3, "boot_reps"))
  expect_null(attr(res3, "boot_id"))
  expect_is(attr(res2, "boot_reps"), "matrix")
  expect_is(attr(res2, "boot_id"), "matrix")
  expect_is(attr(res4, "boot_reps"), "matrix")
  expect_is(attr(res4, "boot_id"), "matrix")
  expect_equal(dim(attr(res2, "boot_reps")), c(nrow(df1), 10))
  expect_equal(dim(attr(res2, "boot_id")), c(nrow(df1), 10))
  expect_equal(dim(attr(res4, "boot_reps")), c(nrow(df1), 10))
  expect_equal(dim(attr(res4, "boot_id")), c(nrow(df1), 10))

})


test_that("resids work for \"lrm\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("rms")

  # Load data
  data(df1)

  # Fit cumulative link model
  fit <- rms::lrm(y ~ x, data = df1)

  # Compute residuals
  res1 <- resids(fit)
  res2 <- resids(fit, nsim = 10)

  # Expectations
  expect_equal(length(res1), nrow(df1))
  expect_equal(length(res2), nrow(df1))
  expect_null(attr(res1, "boot_reps"))
  expect_null(attr(res1, "boot_id"))
  expect_is(attr(res2, "boot_reps"), "matrix")
  expect_is(attr(res2, "boot_id"), "matrix")
  expect_equal(dim(attr(res2, "boot_reps")), c(nrow(df1), 10))
  expect_equal(dim(attr(res2, "boot_id")), c(nrow(df1), 10))

})


test_that("resids work for \"orm\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("rms")

  # Load data
  data(df1)

  # Fit cumulative link model
  fit <- rms::orm(y ~ x, data = df1, family = logistic)

  # Compute residuals
  res1 <- resids(fit)
  res2 <- resids(fit, nsim = 10)

  # Expectations
  expect_equal(length(res1), nrow(df1))
  expect_equal(length(res2), nrow(df1))
  expect_null(attr(res1, "boot_reps"))
  expect_null(attr(res1, "boot_id"))
  expect_is(attr(res2, "boot_reps"), "matrix")
  expect_is(attr(res2, "boot_id"), "matrix")
  expect_equal(dim(attr(res2, "boot_reps")), c(nrow(df1), 10))
  expect_equal(dim(attr(res2, "boot_id")), c(nrow(df1), 10))

})


test_that("resids work for \"polr\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("MASS")

  # Load data
  data(df1)

  # Fit cumulative link model
  fit <- MASS::polr(y ~ x + I(x ^ 2), data = df1, method = "logistic")

  # Compute residuals
  res1 <- resids(fit)
  res2 <- resids(fit, nsim = 10)

  # Expectations
  expect_equal(length(res1), nrow(df1))
  expect_equal(length(res2), nrow(df1))
  expect_null(attr(res1, "boot_reps"))
  expect_null(attr(res1, "boot_id"))
  expect_is(attr(res2, "boot_reps"), "matrix")
  expect_is(attr(res2, "boot_id"), "matrix")
  expect_equal(dim(attr(res2, "boot_reps")), c(nrow(df1), 10))
  expect_equal(dim(attr(res2, "boot_id")), c(nrow(df1), 10))

})


test_that("resids work for \"vglm\" objects", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("rms")

  # Load data
  data(df1)

  # Fit cumulative link model
  suppressWarnings(
    fit <- VGAM::vglm(y ~ x + I(x ^ 2), data = df1,
                      family = VGAM::cumulative(link = "logit",
                                                parallel = TRUE))
  )

  # Compute residuals
  res1 <- resids(fit)
  res2 <- resids(fit, nsim = 10)

  # Expectations
  expect_equal(length(res1), nrow(df1))
  expect_equal(length(res2), nrow(df1))
  expect_null(attr(res1, "boot_reps"))
  expect_null(attr(res1, "boot_id"))
  expect_is(attr(res2, "boot_reps"), "matrix")
  expect_is(attr(res2, "boot_id"), "matrix")
  expect_equal(dim(attr(res2, "boot_reps")), c(nrow(df1), 10))
  expect_equal(dim(attr(res2, "boot_id")), c(nrow(df1), 10))

})


test_that("resids work for \"clm\" objects with different link functions", {

  # Skips
  skip_on_cran()
  skip_if_not_installed("ordinal")

  # Load data
  data(df1)

  # Fit cumulative link models
  fit1 <- ordinal::clm(y ~ x + I(x ^ 2), data = df1, link = "logit")
  fit2 <- ordinal::clm(y ~ x + I(x ^ 2), data = df1, link = "probit")
  fit3 <- ordinal::clm(y ~ x + I(x ^ 2), data = df1, link = "loglog")
  fit4 <- ordinal::clm(y ~ x + I(x ^ 2), data = df1, link = "cloglog")
  fit5 <- ordinal::clm(y ~ x + I(x ^ 2), data = df1, link = "cauchit")

  # Compute residuals
  res1 <- resids(fit1)
  res2 <- resids(fit2)
  res3 <- resids(fit3)
  res4 <- resids(fit4)
  res5 <- resids(fit5)

  # Expectations
  expect_equal(length(res1), nrow(df1))
  expect_equal(length(res2), nrow(df1))
  expect_equal(length(res3), nrow(df1))
  expect_equal(length(res4), nrow(df1))
  expect_equal(length(res5), nrow(df1))

})

