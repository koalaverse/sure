#' Kolmogorov-Smirnov Simulation
#'
#' Simulate p-values from the Kolmogorov-Smirnov test (experimental).
#'
#' @param object An object of class \code{\link[ordinal]{clm}},
#' \code{\link[MASS]{polr}}, or \code{\link[VGAM]{vglm}}.
#'
#' @param nsim Integer specifying the number of bootstrap replicates to use.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @param x An object of class \code{"ksTest"}.
#'
#' @return An object of class \code{c("ksTest", "numeric")}.
#'
#' @rdname ksTest
#' @export
ksTest <- function(object, nsim = 10, ...) {
  if (nsim <- as.integer(nsim) < 2) {
    stop("nsim must be a postive integer >= 2")
  }
  UseMethod("ksTest")
}


#' @rdname ksTest
#' @export
ksTest.clm <- function(object, nsim = 10, ...) {
  res <- resids(object, nsim = nsim)
  pfun <- getDistributionFunction(object)
  pvals <- apply(attr(res, "boot.reps"), MARGIN = 2, FUN = function(x) {
    stats::ks.test(x, y = pfun)$p.value
  })
  class(pvals) <- c("ksTest", "numeric")
  pvals
}


#' @rdname ksTest
#' @export
ksTest.polr <- function(object, nsim = 10, ...) {
  res <- resids(object, nsim = nsim)
  pfun <- getDistributionFunction(object)
  pvals <- apply(attr(res, "boot.reps"), MARGIN = 2, FUN = function(x) {
    stats::ks.test(x, y = pfun)$p.value
  })
  class(pvals) <- c("ksTest", "numeric")
  pvals
}


#' @rdname ksTest
#' @export
ksTest.vglm <- function(object, nsim = 10, ...) {
  res <- resids(object, nsim = nsim)
  pfun <- getDistributionFunction(object)
  pvals <- apply(attr(res, "boot.reps"), MARGIN = 2, FUN = function(x) {
    stats::ks.test(x, y = pfun)$p.value
  })
  class(pvals) <- c("ksTest", "numeric")
  pvals
}


#' @rdname ksTest
#' @export
plot.ksTest <- function(x, ...) {
  graphics::plot(stats::ecdf(x), xlab = "p-value", xlim = c(0, 1), ...)
  graphics::abline(0, 1, lty = 2)
}
