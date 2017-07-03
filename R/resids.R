#' Residuals for Ordinal Regression Models
#'
#' Surrogate and jittered residuals for ordinal regression models.
#'
#' @param object
#'
#' @param type
#'
#' @param nsim Integer specifying the number of bootstrap replicates to use.
#'
#' @return A numeric vector of residuals.
#'
#' @references
#' Liu, Dungang and Zhang, Heping. Residuals and Diagnostics for Ordinal
#' Regression Models: A Surrogate Approach.
#' \emph{Journal of the American Statistical Association} (accepted).
#'
#' @export
resids <- function(object, ...) {
  UseMethod("resids")
}


#' @rdname resids
#' @export
resids.default <- function(object, type = c("surrogate", "jitter"), nsim = 1,
                           ...) {

  # Sanity check
  if (!inherits(object, c("clm", "polr", "vglm"))) {
    stop(deparse(substitute(object)), " should be of class \"clm\", \"polr\", ",
         "or \"vglm\".")
  }

  # Extract number of observations, response values, and truncation bounds
  n.obs <- nobs(object)
  y <- getResponseValues(object)
  bounds <- getBounds(object)

  # Construct residuals
  mr <- getMeanResponse(object)
  res <- getSurrogateResiduals(y, n.obs = n.obs, mean.response = mr,
                               bounds = bounds)
  if (nsim > 1) {  # bootstrap
    boot.res <- boot.index <- matrix(nrow = n.obs, ncol = nsim)
    for(i in seq_len(nsim)) {
      boot.index[, i] <- sample(n.obs, replace = TRUE)
      mr <- getMeanResponse(object)[boot.index[, i]]
      boot.res[, i] <-
        getSurrogateResiduals(y[boot.index[, i]], n.obs = n.obs,
                              mean.response = mr, bounds = bounds)
    }
    attr(res, "boot.reps") <- boot.res
    attr(res, "boot.id") <- boot.index
  }

  # Return residuals
  res

}
