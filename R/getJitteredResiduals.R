#' @keywords internal
getJitteredResiduals <- function(object, y, n.obs, mean.response, jitter.scale,
                                 ...) {
  jitter.scale <- match.arg(jitter.scale)
  if (jitter.scale == "response") {
    runif(n.obs, min = y, max = y + 1) - mean.response
  } else {
    dist.fun <- getDistributionFunction(object)
    runif(n.obs, min = dist.fun(y - 1), max = dist.fun(y)) - mean.response
  }
}
