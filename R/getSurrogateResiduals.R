#' @keywords internal
getSurrogateResiduals <- function(y, n.obs, mean.response, bounds) {
  # TODO: Add support for other distributions!
  # TODO: Avoid for loop, if possible!
  res <- numeric(n.obs)
  for(i in 1:n.obs) {
    res[i] <- truncdist::rtrunc(1, spec = "norm", a = bounds[y[i]],
                                b = bounds[y[i] + 1], mean = mean.response[i],
                                sd = 1) - mean.response[i]
  }
  res
}
