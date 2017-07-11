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
#' @param fit The fitted model. Only required when \code{object} is of class
#' \code{"resid"} and \code{what = "mean"}.
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
#' Default is \code{NULL}.
#'
#' @param ylab Character string giving the text to use for the y-axis label.
#' Default is \code{NULL}.
#'
#' @param main Character string giving the text to use for the plot title.
#' Default is \code{NULL}.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @rdname resplot
#' @export
resplot <- function(object, what = c("qq", "mean", "covariate"), x = NULL, ...) {
  UseMethod("resplot")
}


#' @rdname resplot
#' @export
resplot.resid <- function(object, what = c("qq", "mean", "covariate"),
                          x = NULL, fit = NULL, distribution = qnorm, alpha = 1,
                          xlab = NULL, ylab = NULL, main = NULL, ...) {

  # Number of bootstrap replicates
  if (is.null(attr(object, "boot.reps"))) {
    nsim <- 1
  } else {
    nsim <- ncol(attr(object, "boot.reps"))
  }

  # What type of plot to produce
  what <- match.arg(what)

  # Q-Q plot of the residuals
  if (what == "qq") {
    if (nsim == 1) {
      QQplot(object, distribution = distribution)#xlab = xlab, ylab = ylab, main = main)
    } else {
      QQplot(as.vector(attr(object, "boot.reps")), distribution = distribution)
    }
  }

  # Residual-by-fitted plot
  if (what == "mean") {
    if (is.null(fit)) {
      stop("No fitted values supplied.")
    }
    if (is.null(ylab)) {
      ylab <- "Residual"
    }
    if (is.null(xlab)) {
      xlab <- "Fitted value"
    }
    mr <- getMeanResponse(fit)
    if (nsim == 1) {
      plot(mr, object, xlab = xlab, ylab = ylab, main = main)
      lines(lowess(mr, object), lwd = 2, col = "red")
    } else {
      boot.reps <- as.vector(attr(object, "boot.reps"))
      boot.id <- as.vector(attr(object, "boot.id"))
      plot(mr[boot.id], boot.reps, col = adjustcolor(1, alpha.f = alpha),
           xlab = xlab, ylab = ylab, main = main)
      lines(lowess(rep(mr, times = nsim), boot.reps), lwd = 2, col = "red")
    }
  }

  # Residual-by-covariate plot
  if (what == "covariate") {  # residual-by-covariate
    if (is.null(x)) {
      stop("No covariate supplied.")
    }
    if (is.null(ylab)) {
      ylab <- "Residual"
    }
    if (is.null(xlab)) {
      xlab <- deparse(substitute(x))
    }
    if (nsim == 1) {
      plot(x, object, xlab = xlab, ylab = ylab, main = main)
      lines(lowess(x, object), lwd = 2, col = "red")
      abline(h = c(-2, 2), lty = 2, lwd = 1, col = "red")
    } else {
      boot.reps <- as.vector(attr(object, "boot.reps"))
      boot.id <- as.vector(attr(object, "boot.id"))
      plot(x[boot.id], boot.reps, xlab = xlab, ylab = ylab, main = main)
      lines(lowess(rep(x, times = nsim), boot.reps), lwd = 2, col = "red")
    }
  }
}


#' @rdname resplot
#' @export
resplot.clm <- function(object, what = c("qq", "mean", "covariate"),
                        x = NULL, nsim = 1, alpha = 1, xlab = NULL,
                        ylab = NULL, main = NULL, ...) {
  res <- resids(object, nsim = nsim)
  quan.fun <- getQuantileFunction(object)  # reference distribution
  resplot.resid(res, what = what, x, distribution = quan.fun, fit = object,
                nsim = nsim, alpha = alpha, xlab = xlab, ylab = ylab,
                main = main, ...)
}


#' @rdname resplot
#' @export
resplot.polr <- function(object, what = c("qq", "mean", "covariate"),
                         x = NULL, nsim = 1, alpha = 1, xlab = NULL,
                         ylab = NULL, main = NULL, ...) {
  res <- resids(object, nsim = nsim)
  quan.fun <- getQuantileFunction(object)  # reference distribution
  resplot.resid(res, what = what, x, distribution = quan.fun, fit = object,
                nsim = nsim, alpha = alpha, xlab = xlab, ylab = ylab,
                main = main, ...)
}

#' @rdname resplot
#' @export
resplot.vglm <- function(object, what = c("qq", "mean", "covariate"),
                         x = NULL, nsim = 1, alpha = 1, xlab = NULL,
                         ylab = NULL, main = NULL, ...) {
  res <- resids(object, nsim = nsim)
  quan.fun <- getQuantileFunction(object)  # reference distribution
  resplot.resid(res, what = what, x, distribution = quan.fun, fit = object,
                nsim = nsim, alpha = alpha, xlab = xlab, ylab = ylab,
                main = main, ...)
}
