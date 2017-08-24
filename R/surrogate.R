#' Surrogate Response
#'
#' Simulate surrogate response values for cumulative link regression models
#' using the latent method described in Liu and Zhang (2017).
#'
#' @param object An object of class \code{\link[ordinal]{clm}},
#' \code{\link[rms]{lrm}}, \code{\link[rms]{orm}}, \code{\link[MASS]{polr}}, or
#' \code{\link[VGAM]{vglm}}.
#'
#' @param nsim Integer specifying the number of bootstrap replicates to use.
#' Default is \code{1L} meaning no bootstrap samples.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @return A numeric vector of class \code{c("numeric", "surrogate")} containing
#' the simulated surrogate response values. Additionally, if \code{nsim} > 1,
#' then the result will contain the attributes:
#' \describe{
#'   \item{\code{boot.reps}}{A matrix  with \code{nsim} columns, one for each
#'   bootstrap replicate of the surrogate values. Note, these are random and do
#'   not correspond to the original ordering of the data;}
#'   \item{\code{boot.id}}{A matrix  with \code{nsim} columns. Each column
#'   contains the observation number each surrogate value corresponds to in
#'   \code{boot.reps}. (This is used for plotting purposes.)}
#' }
#'
#' @note
#' Surrogate response values require sampling from a continuous distribution;
#' consequently, the result will be different with every call to
#' \code{surrogate}. The internal functions used for sampling from truncated
#' distributions are based on modified versions of
#' \code{\link[truncdist]{rtrunc}} and \code{\link[truncdist]{qtrunc}}.
#'
#' @references
#' Liu, Dungang and Zhang, Heping. Residuals and Diagnostics for Ordinal
#' Regression Models: A Surrogate Approach.
#' \emph{Journal of the American Statistical Association} (accepted). URL
#' http://www.tandfonline.com/doi/abs/10.1080/01621459.2017.1292915?journalCode=uasa20
#'
#' Nadarajah, Saralees and Kotz, Samuel. R Programs for Truncated Distributions.
#' \emph{Journal of Statistical Software, Code Snippet}, 16(2), 1-8, 2006. URL
#' https://www.jstatsoft.org/v016/c02.
#'
#' @export
surrogate <- function(object, nsim = 1L, ...) {

  # Extract number of observations, response values, and truncation bounds
  y <- getResponseValues(object)
  n.obs <- length(y)
  bounds <- getBounds(object)
  mr <- getMeanResponse(object)  # -f(x; beta) for cumulative link models

  # Simulate surrogate values
  s <- getSurrogate(object, y = y, n.obs = n.obs, mean.response = mr,
                    bounds = bounds)

  # Multiple samples
  if (nsim > 1L) {  # bootstrap
    boot.s <- boot.index <- matrix(nrow = n.obs, ncol = nsim)
    for(i in seq_len(nsim)) {
      boot.index[, i] <- sample(n.obs, replace = TRUE)
      mr <- getMeanResponse(object)[boot.index[, i]]
      boot.s[, i] <-
        getSurrogate(object, y = y[boot.index[, i]], n.obs = n.obs,
                     mean.response = mr, bounds = bounds)
    }
    attr(s, "boot.reps") <- boot.s
    attr(s, "boot.id") <- boot.index
  }

  # Return residuals
  class(s) <- c("numeric", "surrogate")
  s

}


#' @keywords internal
getSurrogate <- function(object, y, n.obs, mean.response, bounds) {
  dist.name <- getDistributionName(object)
  if (dist.name == "norm") {
    .rtrunc(n.obs, spec = "norm", a = bounds[y], b = bounds[y + 1],
            mean = mean.response, sd = 1)
  } else if (dist.name == "logis") {
    .rtrunc(n.obs, spec = "logis", a = bounds[y], b = bounds[y + 1],
            location = mean.response, scale = 1)
  } else if (dist.name == "cauchy") {
    .rtrunc(n.obs, spec = "cauchy", a = bounds[y], b = bounds[y + 1],
            location = mean.response, scale = 1)
  } else if (dist.name == "gumbel") {
    .rtrunc(n.obs, spec = "gumbel", a = bounds[y], b = bounds[y + 1],
            location = mean.response, scale = 1)
  } else if (dist.name == "Gumbel") {
    .rtrunc(n.obs, spec = "Gumbel", a = bounds[y], b = bounds[y + 1],
            location = mean.response, scale = 1)
  } else {
    stop("Distribution not supported.")
  }
}
