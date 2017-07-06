#' @keywords internal
QQplot <- function(y, distribution = qnorm) {
  distribution <- match.fun(distribution)
  x <- distribution(ppoints(length(y)))[order(order(y))]
  qqplot(y, x, main = "Theoretical Quantiles", ylab = "Sample Quantiles")
  qqline(y, distribution = distribution, col = "red")
}
