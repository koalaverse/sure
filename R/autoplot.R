#' Residual plots
#'
#' Residual-based diagnostic plots for cumulative link and general regression
#' models using \code{\link[ggplot2]{ggplot2}} graphics.
#'
#' @param object An object of class \code{\link[ordinal]{clm}},
#' \code{\link[stats]{glm}}, \code{\link[rms]{lrm}}, \code{\link[rms]{orm}},
#' \code{\link[MASS]{polr}}, or \code{\link[VGAM]{vglm}}.
#'
#' @param what Character string specifying what to plot. Default is \code{"qq"}
#' which produces a quantile-quantile plots of the residuals.
#'
#' @param x A vector giving the covariate values to use for residual-by-
#' covariate plots (i.e., when \code{what = "covariate"}).
#'
#' @param fit The fitted model from which the residuals were extracted. (Only
#' required if \code{what = "fitted"} and \code{object} inherits from class
#' \code{"resid"}.)
#'
#' @param distribution Function that computes the quantiles for the reference
#' distribution to use in the quantile-quantile plot. Default is \code{qnorm}
#' which is only appropriate for models using a probit link function. When
#' \code{jitter.scale = "probability"}, the reference distribution is always
#' U(-0.5, 0.5). (Only
#' required if \code{object} inherits from class \code{"resid"}.)
#'
#' @param ncol Integer specifying the number of columns to use for the plot
#' layout (if requesting multiple plots). Default is \code{NULL}.
#'
#' @param alpha A single values in the interval [0, 1] controlling the opacity
#' alpha of the plotted points. Only used when \code{nsim} > 1.
#'
#' @param xlab Character string giving the text to use for the x-axis label in
#' residual-by-covariate plots. Default is \code{NULL}.
#'
#' @param color Character string or integer specifying what color to use for the
#' points in the residual vs fitted value/covariate plot.
#' Default is \code{"black"}.
#'
#' @param shape Integer or single character specifying a symbol to be used for
#' plotting the points in the residual vs fitted value/covariate plot.
#'
#' @param size Numeric value specifying the size to use for the points in the
#' residual vs fitted value/covariate plot.
#'
#' @param qqpoint.color Character string or integer specifying what color to use
#' for the points in the quantile-quantile plot.
#'
#' @param qqpoint.shape Integer or single character specifying a symbol to be
#' used for plotting the points in the quantile-quantile plot.
#'
#' @param qqpoint.size Numeric value specifying the size to use for the points
#' in the quantile-quantile plot.
#'
#' @param qqline.color Character string or integer specifying what color to use
#' for the points in the quantile-quantile plot.
#'
#' @param qqline.linetype Integer or character string (e.g., \code{"dashed"})
#' specifying the type of line to use in the quantile-quantile plot.
#'
#' @param qqline.size Numeric value specifying the thickness of the line in the
#' quantile-quantile plot.
#'
#' @param smooth Logical indicating whether or not too add a nonparametric
#' smooth to certain plots. Default is \code{TRUE}.
#'
#' @param smooth.color Character string or integer specifying what color to use
#' for the nonparametric smooth.
#'
#' @param smooth.linetype Integer or character string (e.g., \code{"dashed"})
#' specifying the type of line to use for the nonparametric smooth.
#'
#' @param smooth.size Numeric value specifying the thickness of the line for the
#' nonparametric smooth.
#'
#' @param fill Character string or integer specifying the color to use to fill
#' the boxplots for residual-by-covariate plots when \code{x} is of class
#' \code{"factor"}. Default is \code{NULL} which colors the boxplots according
#' to the factor levels.
#'
#' @param ... Additional optional arguments to be passed onto
#' \code{\link[sure]{resids}}.
#'
#' @return A \code{"ggplot"} object.
#'
#' @rdname autoplot.resid
#'
#' @export
#'
#' @examples
#' # See ?resids for an example
#' ?resids
autoplot.resid <- function(
  object,
  what = c("qq", "fitted", "covariate"),
  x = NULL,
  fit = NULL,
  distribution = qnorm,
  ncol = NULL,
  alpha = 1,
  xlab = NULL,
  color = "#444444",
  shape = 19,
  size = 2,
  qqpoint.color = "#444444",
  qqpoint.shape = 19,
  qqpoint.size = 2,
  qqline.color = "#888888",
  qqline.linetype = "dashed",
  qqline.size = 1,
  smooth = TRUE,
  smooth.color = "red",
  smooth.linetype = 1,
  smooth.size = 1,
  fill = NULL,
  ...
) {


  # What type of plot to produce
  what <- match.arg(what, several.ok = TRUE)

  # Figure out number of plots and layout
  np <- length(what)
  if (is.null(ncol)) {
    ncol <- length(what)
  }

  # Check that fitted mean response values are available
  if ("fitted" %in% what) {
    if (is.null(fit)) {
      stop("Cannot extract mean response. Please supply the original fitted",
           " model object via the `fit` argument.")
    }
    mr <- getMeanResponse(fit)
  }

  # Check that covariate values are supplied
  if ("covariate"  %in% what) {
    if (is.null(x)) {
      stop("No covariate to plot. Please supply a vector of covariate values",
           " via the `x` argument")
    }
    if (is.null(xlab)) {
      # xlab <- getColumnName(x)
      xlab <- deparse(substitute(x))
    }
  }

  # Deal with bootstrap replicates
  if (is.null(attr(object, "boot_reps"))) {
    nsim <- 1
    res <- object
    if ("qq" %in% what) {
      res.med <- object
    }
  } else {
    res.mat <- attr(object, "boot_reps")
    res <- as.numeric(as.vector(res.mat))
    nsim <- ncol(res.mat)
    if ("qq" %in% what) {
      res.med <- apply(apply(res.mat, MARGIN = 2, FUN = sort,
                             decreasing = FALSE), MARGIN = 1, FUN = median)
    }
    if ("fitted" %in% what) {
      mr <- mr[as.vector(attr(object, "boot_id"))]
    }
    if ("covariate" %in% what) {
      x <- x[as.vector(attr(object, "boot_id"))]
    }
  }

  # Quantile-quantile
  p1 <- if ("qq" %in% what) {
    if (!is.null(attr(object, "jitter.scale"))) {
      if (attr(object, "jitter.scale") == "response") {
        stop("Q-Q plots are not available for jittering on the response scale.")
      }
    }
    distribution <- match.fun(distribution)
    xvals <- distribution(ppoints(length(res.med)))[order(order(res.med))]
    qqline.y <- quantile(res.med, probs = c(0.25, 0.75),
                         names = FALSE, na.rm = TRUE)
    qqline.x <- distribution(c(0.25, 0.75))
    slope <- diff(qqline.y) / diff(qqline.x)
    int <- qqline.y[1L] - slope * qqline.x[1L]
    ggplot(data.frame(x = xvals, y = res.med), aes_string(x = "x", y = "y")) +
      geom_point(color = qqpoint.color, shape = qqpoint.shape,
                 size = qqpoint.size) +
      geom_abline(slope = slope, intercept = int, color = qqline.color,
                  linetype = qqline.linetype, size = qqline.size) +
      labs(x = "Theoretical quantile", y = "Sample quantile")
  } else {
    NULL
  }

  # Residual vs fitted value
  p2 <- if ("fitted" %in% what) {
    p <- ggplot(data.frame("x" = mr, "y" = res), aes_string(x = "x", y = "y")) +
      geom_point(color = color, shape = shape, size = size, alpha = alpha) +
      labs(x = "Fitted value", y = "Surrogate residual")
    if (smooth) {
      p <- p + geom_smooth(color = smooth.color, linetype = smooth.linetype,
                           size = smooth.size, se = FALSE)
    }
    p
  } else {
    NULL
  }

  # Residual vs covariate
  p3 <- if ("covariate" %in% what) {
    p <- ggplot(data.frame("x" = x, "y" = res), aes_string(x = "x", y = "y"))
    if (is.factor(x)) {
      if (is.null(fill)) {
        p <- p + geom_boxplot(aes_string(fill = "x"), alpha = alpha) +
          guides(fill = FALSE)
      } else {
        p <- p + geom_boxplot()
      }
    } else {
      p <- p + geom_point(color = color, shape = shape, size = size,
                          alpha = alpha)
      if (smooth) {
        p <- p + geom_smooth(color = smooth.color, linetype = smooth.linetype,
                             size = smooth.size, se = FALSE)
      }
    }
    p + labs(x = xlab, y = "Surrogate residual")
  } else {
    NULL
  }

  # Return plot(s)
  if (length(what) == 1) {  # return a single plot
    if (what == "qq") {
      p1
    } else if (what == "fitted") {
      p2
    } else {
      p3
    }
  } else {  # return multiple plots
    plots <- list(p1, p2, p3)
    grid.arrange(grobs = plots[!unlist(lapply(plots, FUN = is.null))],
                 ncol = ncol)
  }

}


#' @rdname autoplot.resid
#'
#' @export
autoplot.default <- function(
  object,
  what = c("qq", "fitted", "covariate"),
  x = NULL,
  fit = NULL,
  distribution = qnorm,
  ncol = NULL,
  alpha = 1,
  xlab = NULL,
  color = "#444444",
  shape = 19,
  size = 2,
  qqpoint.color = "#444444",
  qqpoint.shape = 19,
  qqpoint.size = 2,
  qqline.color = "#888888",
  qqline.linetype = "dashed",
  qqline.size = 1,
  smooth = TRUE,
  smooth.color = "red",
  smooth.linetype = 1,
  smooth.size = 1,
  fill = NULL,
  ...
) {

  # Compute residuals
  res <- resids(object, ...)

  # Quantile function to use for Q-Q plots
  qfun <- if (is.null(attr(res, "jitter.scale"))) {
    getQuantileFunction(object)
  } else {
    if (what == "qq" && attr(res, "jitter.scale") == "response") {
      stop("Quantile-quantile plots are not appropriate for residuals ",
           "obtained by jittering on the response scale.")
    }
    function(p) qunif(p, min = -0.5, max = 0.5)
  }

  # Default x-axis label
  if (is.null(xlab)) {
    xlab <- deparse(substitute(x))
  }

  # Call the default method
  autoplot.resid(
    res, what = what, x = x, distribution = qfun, fit = object, ncol = ncol,
    alpha = alpha, xlab = xlab, color = color, shape = shape, size = size,
    qqpoint.color = qqpoint.color, qqpoint.shape = qqpoint.shape,
    qqpoint.size = qqpoint.size, qqline.color = qqline.color,
    qqline.linetype = qqline.linetype, qqline.size = qqline.size,
    smooth = smooth, smooth.color = smooth.color,
    smooth.linetype = smooth.linetype, smooth.size = smooth.size, fill = fill
  )

}
