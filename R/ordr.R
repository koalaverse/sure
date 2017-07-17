#' ordr: An R package for constructing residuals and diagnostics for ordinal
#' regression models.
#'
#' The \code{ordr} package provides surrogate-based residuals for fitted ordinal
#' regression models of class \code{\link[ordinal]{clm}},
#' \code{\link[MASS]{polr}}, and \code{\link[VGAM]{vglm}}.
#'
#' The development version can be found on GitHub:
#' https://github.com/AFIT-R/ordr. As of right now, \code{ordr} only exports two
#' functions:
#' \itemize{
#'   \item{\code{resids}} - construct (surrogate-based) residuals for
#'   \code{\link[ordinal]{clm}}, \code{\link[MASS]{polr}}, and
#'   \code{\link[VGAM]{vglm}} objects;
#'   \item{\code{autoplot}} - plot diagnostics for \code{\link[ordinal]{clm}},
#' \code{\link[MASS]{polr}}, and \code{\link[VGAM]{vglm}} objects using
#' \code{\link[ggplot2]{ggplot2}} graphics;
#' }
#'
#' @references
#' Liu, Dungang and Zhang, Heping. Residuals and Diagnostics for Ordinal
#' Regression Models: A Surrogate Approach.
#' \emph{Journal of the American Statistical Association} (accepted).
#'
#' @importFrom ggplot2  aes_string geom_abline geom_boxplot geom_point
#' @importFrom ggplot2 geom_smooth ggplot ggtitle guides xlab ylab
#' @importFrom stats .checkMFClasses lowess median model.frame model.matrix
#' @importFrom stats model.response nobs pcauchy plogis pnorm ppoints
#' @importFrom stats qcauchy qlogis qnorm qqline qqplot qqnorm quantile runif
#'
#' @docType package
#' @name ordr
NULL
