#' Residuals for Ordinal Regression Models
#'
#' Surrogate and jittered residuals for ordinal regression models.
#'
#' @param object An object of class \code{\link[ordinal]{clm}},
#' \code{\link[MASS]{polr}}, or \code{\link[VGAM]{vglm}}.
#'
#' @param type Character string specifying the type of residuals to construct.
#' Should be one of \code{"surrogate"} or \code{"jitter"}. Default is
#' \code{"surrogate"}. (Currently ignored.)
#'
#' @param jitter.scale Character string specifying the scale on which to perform
#' the jittering. Should be one of \code{"response"} or \code{"probability"}.
#' Only used when \code{type = "jitter"}. Default is \code{"response"}.
#' (Currently ignored.)
#'
#' @param nsim Integer specifying the number of bootstrap replicates to use.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @return A numeric vector (\code{nsim = 1}) or matrix (\code{nsim} > 1) of
#' residuals. If \code{nsim} > 1, then the result will be a matrix with
#' \code{nsim} columns, one for each bootstrap repliacte of the residuals. The
#' result will contain the additional class \code{"resid"}, which is useful for
#' plotting.
#'
#' @note The internal functions used for sampling from truncated distirbutions
#' are based on modified versions of \code{\link[truncdist]{rtrunc}} and
#' \code{\link[truncdist]{qtrunc}}.
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
resids <- function(object, type, jitter.scale, nsim, ...) {
  UseMethod("resids")
}


#' @rdname resids
#' @export
resids.default <- function(object, type = c("surrogate", "jitter"),
                           jitter.scale = c("response", "probability"),
                           nsim = 1, ...) {

  # Sanity check
  if (!inherits(object, c("clm", "polr", "vglm"))) {
    stop(deparse(substitute(object)), " should be of class \"clm\", \"polr\", ",
         "or \"vglm\".")
  }

  # Extract number of observations, response values, and truncation bounds
  y <- getResponseValues(object)
  n.obs <- length(y)
  bounds <- getBounds(object)

  # What type of residuals?
  type <- match.arg(type)
  jitter.scale <- match.arg(jitter.scale)
  if (jitter.scale == "probability") {  # throw error, for now!
    stop("Jittering on the probability scale is not yet implemented.")
  }

  # Construct residuals
  mr <- getMeanResponse(object)
  res <- getResiduals(object, type = type, jitter.scale = jitter.scale, y = y,
                      n.obs = n.obs, mean.response = mr, bounds = bounds)
  if (nsim > 1) {  # bootstrap
    boot.res <- boot.index <- matrix(nrow = n.obs, ncol = nsim)
    for(i in seq_len(nsim)) {
      boot.index[, i] <- sample(n.obs, replace = TRUE)
      mr <- getMeanResponse(object)[boot.index[, i]]
      boot.res[, i] <-
        getResiduals(object, type = type, jitter.scale = jitter.scale,
                     y = y[boot.index[, i]], n.obs = n.obs, mean.response = mr,
                     bounds = bounds)
    }
    attr(res, "boot.reps") <- boot.res
    attr(res, "boot.id") <- boot.index
  }

  # Return residuals
  class(res) <- c("numeric", "resid")
  res

}


#' @keywords internal
#' @export
print.resid <- function(x, ...) {
  attributes(x) <- NULL
  print(x, ...)
}
