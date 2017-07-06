#' Residual Plots for Ordinal Regression Models
#'
#' Residual-based diagnostic plots for ordnial regression models.
#'
#' @param object An object of class \code{\link[ordinal]{clm}},
#' \code{\link[MASS]{polr}}, or \code{\link[VGAM]{vglm}}.
#'
#' @param what Character string specifying what to plot. Default is \code{"qq"}
#' which produces a quantile-quantile plots of the residuals.
#'
#' @param x A vector giving the covariate values to use for residual-by-
#' covariate plots (i.e., when \code{what = "covariate"}).
#'
#' @param distribution Function that computes the quantiles for the reference
#' distribution to use in the quantile-quantile plot. Default is \code{qnorm}
#' which is only appropriate for models using a probit link function.
#'
#' @param nsim Integer specifying the number of bootstrap replicates to use.
#'
#' @param alpha A single values in the interval [0, 1] controlling the opacity
#' alpha of the plotted points. Only used when \code{nsim} > 1.
#'
#' @param xlab Character string giving the text to use for the x-axis label.
#' Default is \code{NULL}
#'
#' @param ylab Character string giving the text to use for the y-axis label.
#'
#' @param main Character string giving the text to use for the plot title.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @rdname resplot
#' @export
resplot <- function(object, what = c("qq", "covariate"), x = NULL, ...) {
  UseMethod("resplot")
}


#' @rdname resplot
#' @export
resplot.resid <- function(object, what = c("qq", "covariate"), x = NULL,
                          distribution = qnorm, alpha = 1, xlab = NULL,
                          ylab = NULL, main = NULL, ...)
  {
  if (is.null(attr(object, "boot.reps"))) {
    nsim <- 1
  } else {
    nsim <- ncol(attr(object, "boot.reps"))
  }
  what <- match.arg(what)
  if (what == "qq") {  # Q-Q plot
    if (nsim == 1) {
      QQplot(object, distribution = distribution)#xlab = xlab, ylab = ylab, main = main)
    } else {
      QQplot(as.numeric(attr(object, "boot.reps")), distribution = distribution)
    }
  } else if (what == "covariate") {  # residual-by-covariate
    if (is.null(x)) {
      stop("No covariate supplied.")
    }
    if (is.null(ylab)) {
      ylab <- "residual"
    }
    if (nsim == 1) {
      plot(x, object, xlab = xlab, ylab = ylab, main = main)
      lines(lowess(x, object), lwd = 2, col = "red")
      abline(h = c(-2, 2), lty = 2, lwd = 1, col = "red")
    } else {
      plot(x, object, xlab = xlab, ylab = ylab, main = main)  # original residuals w/ bootstrap reps
      boot.reps <- attr(object, "boot.reps")
      boot.id <- attr(object, "boot.id")
      for (i in seq_len(nsim)) {  # add bootstrap residuals
        points(x[boot.id[, i]], boot.reps[, i],
               col = adjustcolor(1, alpha.f = alpha))
      }
      xx <- rep(x, times = nsim)
      lines(lowess(xx, as.numeric(boot.reps)), lwd = 2, col = "red")
    }
    abline(h = c(-2, 2), lty = 2, lwd = 1, col = "red")
  }
}


#' @rdname resplot
#' @export
resplot.vglm <- function(object, what = c("qq", "covariate"), x = NULL,
                         nsim = 1, alpha = 1, xlab = NULL, ylab = NULL,
                         main = NULL, ...) {
  res <- resids(object, nsim = nsim)
  dist.fun <- getDistributionFunction(object)  # reference distribution
  resplot.resid(res, what = what, x, distribution = dist.fun, nsim = nsim,
                alpha = alpha, xlab = xlab, ylab = ylab, main = main, ...)
}
