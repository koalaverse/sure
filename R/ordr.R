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
#' @importFrom graphics abline lines plot points
#' @importFrom grDevices adjustcolor
#' @importFrom stats .checkMFClasses lowess model.frame model.matrix
#' @importFrom stats model.response nobs pnorm ppoints qcauchy qlogis qnorm
#' @importFrom stats qqline qqplot qqnorm runif
#'
#' @docType package
#' @name ordr
NULL
