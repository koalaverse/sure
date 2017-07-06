#' @keywords internal
getSurrogateResiduals <- function(object, y, n.obs, mean.response, bounds) {
  # TODO: Add support for other distributions!
  # TODO: Avoid for loop, if possible!
  # res <- numeric(n.obs)
  # for(i in 1:n.obs) {
  #   res[i] <- truncdist::rtrunc(1, spec = "norm", a = bounds[y[i]],
  #                               b = bounds[y[i] + 1], mean = mean.response[i],
  #                               sd = 1) - mean.response[i]
  # }
  dist.name <- getDistributionName(object)
  if (dist.name == "norm") {
    .rtrunc(n.obs, spec = "norm", a = bounds[y], b = bounds[y + 1],
            mean = mean.response, sd = 1) - mean.response
  } else if (dist.name == "logis") {
    .rtrunc(n.obs, spec = "logis", a = bounds[y], b = bounds[y + 1],
            location = mean.response, scale = 1) - mean.response
  } else {
    stop("Distribution not supported.")
  }

}


# The following function have been taken from truncdist, but have been modified
# to not throw warnings when vectors are passed to arguments a and b

#' @keywords internal
.rtrunc <- function (n, spec, a = -Inf, b = Inf, ...) {
  .qtrunc(runif(n, min = 0, max = 1), spec, a = a, b = b, ...)
}


#' @keywords internal
.qtrunc <- function (p, spec, a = -Inf, b = Inf, ...) {
  tt <- p
  G <- get(paste("p", spec, sep = ""), mode = "function")
  Gin <- get(paste("q", spec, sep = ""), mode = "function")
  G.a <- G(a, ...)
  G.b <- G(b, ...)
  pmin(pmax(a, Gin(G(a, ...) + p * (G(b, ...) - G(a, ...)), ...)), b)
}
