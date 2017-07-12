#' @keywords internal
getResiduals <- function(object, type, jitter.scale, y, n.obs, mean.response,
                         bounds) {
  if (type == "surrogate") {
    getSurrogateResiduals(object, y = y, n.obs = n.obs,
                          mean.response = mean.response,
                          bounds = bounds)
  } else {
    getJitteredResiduals(object, y = y, n.obs = n.obs,
                         mean.response = mean.response,
                         jitter.scale = jitter.scale)
  }
}


#' @keywords internal
getSurrogateResiduals <- function(object, y, n.obs, mean.response, bounds) {
  dist.name <- getDistributionName(object)
  if (dist.name == "norm") {
    .rtrunc(n.obs, spec = "norm", a = bounds[y], b = bounds[y + 1],
            mean = mean.response, sd = 1) - mean.response
  } else if (dist.name == "logis") {
    .rtrunc(n.obs, spec = "logis", a = bounds[y], b = bounds[y + 1],
            location = mean.response, scale = 1) - mean.response
  } else if (dist.name == "cauchy") {
    .rtrunc(n.obs, spec = "cauchy", a = bounds[y], b = bounds[y + 1],
            location = mean.response, scale = 1) - mean.response
  } else if (dist.name == "gumbel") {
    .rtrunc(n.obs, spec = "gumbel", a = bounds[y], b = bounds[y + 1],
            location = mean.response, scale = 1) - mean.response
  } else if (dist.name == "Gumbel") {
    .rtrunc(n.obs, spec = "Gumbel", a = bounds[y], b = bounds[y + 1],
            location = mean.response, scale = 1) - mean.response
  } else {
    stop("Distribution not supported.")
  }
}


#' @keywords internal
getJitteredResiduals <- function(object, y, n.obs, mean.response, jitter.scale)
  {
  if (jitter.scale == "response") {
    runif(n.obs, min = y, max = y + 1) - mean.response
  } else {
    dist.fun <- getDistributionFunction(object)
    runif(n.obs, min = dist.fun(y - 1), max = dist.fun(y)) - mean.response
  }
}


# The following functions have been taken from truncdist, but have been modified
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
