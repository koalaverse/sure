#' Residual Plots for Ordinal Regression Models
#'
#' Residual-based diagnostic plots for ordnial regression models using
#' \code{\link[ggplot2]{ggplot2}} graphics.
#'
#' @param object An object of class \code{\link[ordinal]{clm}},
#' \code{\link[MASS]{polr}}, \code{\link[VGAM]{vglm}}, or \code{"resid"}.
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
#' @param xlab Character string giving the text to use for the x-axis label in
#' residual-by-covariate plots. Default is \code{NULL}.
#'
#' @param ylab Character string giving the text to use for the y-axis label.
#' Default is \code{NULL}.
#'
#' @param main Character string giving the text to use for the plot title.
#' Default is \code{NULL}.
#'
#' @param col Character string or integer specifying what color to use for the
#' points in the quantile-quantile and residual-by-fitted value/covariate plots.
#' Defaul is \code{"black"}.
#'
#' @param smooth Logical indicating whether or not too add a nonparametric
#' smooth on certain plots. Default is \code{TRUE}.
#'
#' @param smooth.col Character string or integer specifying what color to use
#' for the nonparametric smooth. Default is \code{"blue"}.
#'
#' @param fill Character string or integer specifying the color to use to fill
#' the boxplots for residual-by-covariate plots when \code{x} is of class
#' \code{"factor"}. Default is \code{NULL} which colors the boxplots according
#' to the factor levels.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @return A \code{"ggplot"} object.
#'
#' @export
#'
#' @rdname autoplot.resid
autoplot.resid <- function(object, what = c("qq", "mean", "covariate"),
                           x = NULL, fit = NULL, distribution = qnorm,
                           alpha = 1, xlab = NULL,
                           color = "#444444",
                           qqpoint.color = "#444444",
                           qqpoint.shape = 19,
                           qqpoint.size = 2,
                           qqline.color = "#888888",
                           qqline.linetype = "dashed",
                           qqline.size = 1,
                           smooth = TRUE,
                           smooth.color = "#0000FF",
                           smooth.linetype = 1,
                           smooth.size = 1,
                           fill = NULL, ...) {

  # What type of plot to produce
  what <- match.arg(what)

  # Sanity checks
  if (what == "mean") {
    if (is.null(fit)) {
      stop("Cannot extract mean response. Please supply the original fitted",
           " model object via the `fit` argument.")
    }
    x <- getMeanResponse(fit)
  }
  if (what == "covariate") {
    if (is.null(x)) {
      stop("No covariate to plot. Please supply a vector of covariate values",
           " via the `x` argument")
    }
    if (is.null(xlab)) {
      xlab <- deparse(substitute(x))
    }
  }

  # Number of bootstrap replicates
  if (is.null(attr(object, "boot.reps"))) {
    nsim <- 1
    res <- object
  } else {
    res.mat <- attr(object, "boot.reps")
    nsim <- ncol(res.mat)
    res <- as.vector(res.mat)
    x <- x[as.vector(attr(object, "boot.id"))]
  }

  # Quantile-quantile
  if (what == "qq") {
    distribution <- match.fun(distribution)
    x <- distribution(ppoints(length(res)))[order(order(res))]
    qqline.y <- quantile(res, probs = c(0.25, 0.75), names = FALSE,
                         na.rm = TRUE)
    qqline.x <- distribution(c(0.25, 0.75))
    slope <- diff(qqline.y) / diff(qqline.x)
    int <- qqline.y[1L] - slope * qqline.x[1L]
    p <- ggplot(data.frame(x = x, y = res), aes_string(x = "x", y = "y")) +
      geom_point(color = qqpoint.color, shape = qqpoint.shape,
                 size = qqpoint.size) +
      geom_abline(slope = slope, intercept = int, color = qqline.color,
                  linetype = qqline.linetype, size = qqline.size) +
      xlab("Theoretical quantile") +
      ylab("Sample quantile") +
      ggtitle("Quantile-quantile")
  }

  # Residual vs fitted value
  if (what == "mean") {
    p <- ggplot(data.frame(x = x, y = res), aes_string(x = "x", y = "y")) +
      geom_point(color = color, alpha = alpha) +
      xlab("Fitted value") +
      ylab("Residual") +
      ggtitle("Residual vs fitted value")
      if (smooth) {
        p <- p + geom_smooth(color = smooth.color, linetype = smooth.linetype,
                             size = smooth.size, se = FALSE)
      }
  }

  # Residual vs covariate
  if (what == "covariate") {
    p <- ggplot(data.frame(x = x, y = res), aes_string(x = "x", y = "y"))
    if (is.factor(x)) {
      if (is.null(fill)) {
        p <- p + geom_boxplot(aes_string(fill = "x"), alpha = alpha) +
          guides(fill = FALSE)
      } else {
        p <- p + geom_boxplot()
      }
    } else {
      p <- p + geom_point(color = color, alpha = alpha)
      if (smooth) {
        p <- p + geom_smooth(color = smooth.color, linetype = smooth.linetype,
                             size = smooth.size, se = FALSE)
      }
    }
    p <- p +
      xlab(xlab) +
      ylab("Residual") +
      ggtitle("Residual vs covariate")
  }

  # Return plot
  p

}


#' @rdname autoplot.resid
#' @export
autoplot.clm <- function(object, what = c("qq", "mean", "covariate"),
                         x = NULL, nsim = 1, alpha = 1, xlab = NULL,
                         ylab = NULL, color = "#444444",
                         qqpoint.color = "#444444",
                         qqpoint.shape = 19,
                         qqpoint.size = 2,
                         qqline.color = "#888888",
                         qqline.linetype = "dashed",
                         qqline.size = 1,
                         smooth = TRUE,
                         smooth.color = "#0000FF",
                         smooth.linetype = 1,
                         smooth.size = 1,
                         fill = NULL, ...) {
  res <- resids(object, nsim = nsim)
  qfun <- getQuantileFunction(object)  # reference distribution
  if (is.null(xlab)) {
    xlab <- deparse(substitute(x))
  }
  autoplot.resid(res, what = what, x = x, distribution = qfun, fit = object,
                 nsim = nsim, alpha = alpha, xlab = xlab, color = color,
                 qqpoint.color = qqpoint.color,
                 qqpoint.shape = qqpoint.shape,
                 qqpoint.size = qqpoint.size,
                 qqline.color = qqline.color,
                 qqline.linetype = qqline.linetype,
                 qqline.size = qqline.size,
                 smooth = smooth,
                 smooth.color = smooth.color,
                 smooth.linetype = smooth.linetype,
                 smooth.size = smooth.size,
                 fill = fill, ...)
}


#' @rdname autoplot.resid
#' @export
autoplot.polr <- function(object, what = c("qq", "mean", "covariate"),
                          x = NULL, nsim = 1, alpha = 1, xlab = NULL,
                          ylab = NULL, color = "#444444",
                          qqpoint.color = "#444444",
                          qqpoint.shape = 19,
                          qqpoint.size = 2,
                          qqline.color = "#888888",
                          qqline.linetype = "dashed",
                          qqline.size = 1,
                          smooth = TRUE,
                          smooth.color = "#0000FF",
                          smooth.linetype = 1,
                          smooth.size = 1,
                          fill = NULL, ...) {
  res <- resids(object, nsim = nsim)
  qfun <- getQuantileFunction(object)  # reference distribution
  if (is.null(xlab)) {
    xlab <- deparse(substitute(x))
  }
  autoplot.resid(res, what = what, x = x, distribution = qfun, fit = object,
                 nsim = nsim, alpha = alpha, xlab = xlab, color = color,
                 qqpoint.color = qqpoint.color,
                 qqpoint.shape = qqpoint.shape,
                 qqpoint.size = qqpoint.size,
                 qqline.color = qqline.color,
                 qqline.linetype = qqline.linetype,
                 qqline.size = qqline.size,
                 smooth = smooth,
                 smooth.color = smooth.color,
                 smooth.linetype = smooth.linetype,
                 smooth.size = smooth.size,
                 fill = fill, ...)
}


#' @rdname autoplot.resid
#' @export
autoplot.vglm <- function(object, what = c("qq", "mean", "covariate"),
                          x = NULL, nsim = 1, alpha = 1, xlab = NULL,
                          ylab = NULL, color = "#444444",
                          qqpoint.color = "#444444",
                          qqpoint.shape = 19,
                          qqpoint.size = 2,
                          qqline.color = "#888888",
                          qqline.linetype = "dashed",
                          qqline.size = 1,
                          smooth = TRUE,
                          smooth.color = "#0000FF",
                          smooth.linetype = 1,
                          smooth.size = 1,
                          fill = NULL, ...) {
  res <- resids(object, nsim = nsim)
  qfun <- getQuantileFunction(object)  # reference distribution
  if (is.null(xlab)) {
    xlab <- deparse(substitute(x))
  }
  autoplot.resid(res, what = what, x = x, distribution = qfun, fit = object,
                 nsim = nsim, alpha = alpha, xlab = xlab, color = color,
                 qqpoint.color = qqpoint.color,
                 qqpoint.shape = qqpoint.shape,
                 qqpoint.size = qqpoint.size,
                 qqline.color = qqline.color,
                 qqline.linetype = qqline.linetype,
                 qqline.size = qqline.size,
                 smooth = smooth,
                 smooth.color = smooth.color,
                 smooth.linetype = smooth.linetype,
                 smooth.size = smooth.size,
                 fill = fill, ...)
}
