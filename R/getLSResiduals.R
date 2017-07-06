#' @keywords internal
getLSResiduals <- function(object) {
  bounds <- getBounds(object)
  y <- getResponseValues(object)
  mr <- getMeanResponse(object)
  pnorm(bounds[y + 1] - mr) + pnorm(bounds[y] - mr) - 1
}
