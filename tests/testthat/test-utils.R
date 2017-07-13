context("Utility functions")


test_that("ncat works for \"polr\" objects", {
  expect_equal(ncat(fit.clm), 3)
  expect_equal(ncat(fit.polr), 3)
  expect_equal(ncat(fit.vglm), 3)
})
