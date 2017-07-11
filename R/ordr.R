#' ordr: An R package for constructing residuals and diagnostics for ordinal
#' regression models.
#'
#' The \code{ordr} package provides surrogate and jittered residuals for fitted
#' ordinal regression models of class \code{\link[ordinal]{clm}},
#' \code{\link[MASS]{polr}}, and \code{\link[VGAM]{vglm}}.
#'
#' @references
#' Liu, Dungang and Zhang, Heping. Residuals and Diagnostics for Ordinal
#' Regression Models: A Surrogate Approach.
#' \emph{Journal of the American Statistical Association} (accepted).
#'
#' @importFrom ggplot2  aes geom_abline geom_boxplot geom_point geom_smooth
#' @importFrom ggplot2 ggplot ggtitle guides
#' @importFrom graphics abline lines plot points
#' @importFrom grDevices adjustcolor
#' @importFrom stats .checkMFClasses lowess model.frame model.matrix
#' @importFrom stats model.response nobs pcauchy plogis pnorm ppoints
#' @importFrom stats qcauchy qlogis qnorm qqline qqplot qqnorm quantile runif
#'
#' @docType package
#' @name ordr
NULL
