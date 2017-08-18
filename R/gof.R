#' Kolmogorov-Smirnov Simulation
#'
#' Simulate p-values from a goodness-of-fit test (experimental).
#'
#' @param object An object of class \code{\link[ordinal]{clm}},
#' \code{\link[stats]{glm}}, \code{\link[rms]{lrm}}, \code{\link[rms]{orm}},
#' \code{\link[MASS]{polr}}, or \code{\link[VGAM]{vglm}}.
#'
#' @param nsim Integer specifying the number of bootstrap replicates to use.
#'
#' @param test Character string specifying which goodness-of-fit test to use.
#' Current options include: \code{"ks"} for the Kolmogorov-Smirnov test,
#' \code{"ad"} for the Anderson-Darling test, and \code{"cvm"} for the
#' Cramer-Von Mises test. Default is \code{"ks"}.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @param x An object of class \code{"gof"}.
#'
#' @return A numeric vector of class \code{"gof", "numeric"} containing the
#' simulated p-values.
#'
#' @details
#' Under the null hypothesis, the distribution of the p-values should appear
#' uniformly distributed on the interval [0, 1]. This can be visually
#' investigated using the \code{plot} method. A 45 degree line is indicative of
#' a "good" fit.
#'
#' @rdname gof
#'
#' @export
#'
#' @examples
#' # See ?resids for an example
#' ?resids
gof <- function(object, nsim = 10, test = c("ks", "ad", "cvm"), ...) {
  if (nsim <- as.integer(nsim) < 2) {
    stop("nsim must be a postive integer >= 2")
  }
  UseMethod("gof")
}


#' @rdname gof
#' @export
gof.default <- function(object, nsim = 10, test = c("ks", "ad", "cvm"), ...) {
  res <- resids(object, nsim = nsim)
  test <- match.arg(test)
  pfun <- getDistributionFunction(object)
  sim.pvals(res, test = test, pfun = pfun)
}


#' @rdname gof
#' @export
plot.gof <- function(x, ...) {
  graphics::plot(stats::ecdf(x), xlab = "p-value", xlim = c(0, 1), ...)
  graphics::abline(0, 1, lty = 2)
}


#' @keywords internal
sim.pvals <- function(res, test, pfun) {
  gof.test <- switch(test, "ks" = stats::ks.test, "ad" = goftest::ad.test,
                     "cvm" = goftest::cvm.test)
  pvals <- apply(attr(res, "boot.reps"), MARGIN = 2, FUN = function(x) {
    gof.test(x, pfun)$p.value
  })
  class(pvals) <- c("gof", "numeric")
  pvals
}
