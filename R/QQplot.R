#' @keywords internal
QQplot <- function(y, distribution = qnorm) {
  distribution <- match.fun(distribution)
  x <- distribution(ppoints(length(y)))[order(order(y))]
  qqplot(y, x, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
         main = "Q-Q Plot")
  qqline(y, distribution = distribution, col = "red")
}
