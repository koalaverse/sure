#' sure: An R package for constructing surrogate-based residuals and diagnostics
#' for ordinal and general regression models.
#'
#' The \code{sure} package provides surrogate-based residuals for fitted ordinal
#' and general (e.g., binary) regression models of class
#' \code{\link[ordinal]{clm}}, \code{\link[stats]{glm}},
#' \code{\link[MASS]{polr}}, and \code{\link[VGAM]{vglm}}.
#'
#' The development version can be found on GitHub:
#' https://github.com/AFIT-R/sure. As of right now, \code{sure} only exports two
#' functions:
#' \itemize{
#'   \item{\code{resids}} - construct (surrogate-based) residuals for
#'   \code{\link[ordinal]{clm}}, \code{\link[stats]{glm}},
#'   \code{\link[MASS]{polr}}, and \code{\link[VGAM]{vglm}} objects;
#'   \item{\code{autoplot}} - plot diagnostics for \code{\link[ordinal]{clm}},
#' \code{\link[stats]{glm}}, \code{\link[MASS]{polr}}, and
#' \code{\link[VGAM]{vglm}} objects using \code{\link[ggplot2]{ggplot2}}
#' graphics;
#'   \item{\code{gof}} - simulate p-values from a goodness-of-fit test.
#' }
#'
#' @references
#' Liu, Dungang and Zhang, Heping. Residuals and Diagnostics for Ordinal
#' Regression Models: A Surrogate Approach.
#' \emph{Journal of the American Statistical Association} (accepted).
#'
#' @importFrom ggplot2  aes_string geom_abline geom_boxplot geom_point
#'
#' @importFrom ggplot2 geom_smooth ggplot ggtitle guides labs xlab ylab
#'
#' @importFrom stats .checkMFClasses lowess median model.frame model.matrix
#'
#' @importFrom stats model.response nobs pbinom pcauchy plogis pnorm ppoints
#'
#' @importFrom stats predict qcauchy qlogis qnorm qqline qqplot qqnorm quantile
#'
#' @importFrom stats qunif runif
#'
#' @docType package
#'
#' @name sure
NULL
