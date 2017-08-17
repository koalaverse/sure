#' Residuals Cumulative Link and General Regression Models
#'
#' Surrogate-based residuals for cumulative link and general regression models.
#'
#' @param object An object of class \code{\link[ordinal]{clm}},
#' \code{\link[stats]{glm}}, \code{\link[rms]{lrm}}, \code{\link[rms]{orm}},
#' \code{\link[MASS]{polr}}, or \code{\link[VGAM]{vglm}}.
#'
#' @param jitter.scale Character string specifying the scale on which to perform
#' the jittering. Should be one of \code{"probability"} or \code{"response"}.
#' Currently only used when object inherits from class \code{"glm"}. Default is
#' \code{"probability"}.
#'
#' @param nsim Integer specifying the number of bootstrap replicates to use.
#' Default is \code{1L} meaning no bootstrap samples.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @return A numeric vector (\code{nsim = 1}) or matrix (\code{nsim} > 1) of
#' residuals. If \code{nsim} > 1, then the result will be a matrix with
#' \code{nsim} columns, one for each bootstrap repliacte of the residuals. The
#' result will contain the additional class \code{"resid"}, which is recognized
#' by \code{\link{autoplot.resid}}.
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
resids <- function(object, ...) {
  UseMethod("resids")
}


#' @rdname resids
#' @export
resids.default <- function(object, nsim = 1L, ...) {

  # Sanity check
  if (!inherits(object, c("clm", "glm", "lrm", "orm", "polr", "vglm"))) {
    stop(deparse(substitute(object)), " should be of class \"clm\", \"glm\", ",
         "\"lrm\", \"orm\", \"polr\", or \"vglm\".")
  }



  # Extract number of observations, response values, and truncation bounds
  y <- getResponseValues(object)
  n.obs <- length(y)
  bounds <- getBounds(object)
  mr <- getMeanResponse(object)  # -f(x; beta) for cumulative link models

  # Construct residuals
  res <- getSurrogateResiduals(object, y = y, n.obs = n.obs, mean.response = mr,
                               bounds = bounds)

  # Multiple samples
  if (nsim > 1L) {  # bootstrap
    boot.res <- boot.index <- matrix(nrow = n.obs, ncol = nsim)
    for(i in seq_len(nsim)) {
      boot.index[, i] <- sample(n.obs, replace = TRUE)
      mr <- getMeanResponse(object)[boot.index[, i]]
      boot.res[, i] <-
        getSurrogateResiduals(object, y = y[boot.index[, i]], n.obs = n.obs,
                              mean.response = mr, bounds = bounds)
    }
    attr(res, "boot.reps") <- boot.res
    attr(res, "boot.id") <- boot.index
  }

  # Return residuals
  class(res) <- c("numeric", "resid")
  res

}


#' @rdname resids
#' @export
resids.lrm <- function(object, nsim = 1L, ...) {
  resids.default(object, nsim = nsim, ...)
}


#' @rdname resids
#' @export
resids.glm <- function(object, jitter.scale = c("probability", "response"),
                       nsim = 1L, ...) {

  # Print warning message
  warning("Using sure with \"glm\" objects is still experimental. Use at ",
          "your own risk.")

  # Check for binomial family
  if (object$family$family != "binomial") {
    stop("Jittering is only available for \"glm\" objects with the binomial ",
         "family.")
  }

  # What type of residuals?
  jitter.scale <- match.arg(jitter.scale)

  # Extract response values and number of observations
  # FIXME: What about missing values?
  # FIXME: What about matrix response, etc.?
  y <- getResponseValues(object)  # should be in {0, 1}
  n.obs <- length(y)

  # Construct residuals
  res <- getJitteredResiduals(object, jitter.scale = jitter.scale, y = y)

  # Assign attribute to help in plotting
  attr(res, "jitter.scale") <- jitter.scale

  # Multiple samples
  if (nsim > 1L) {  # bootstrap
    boot.res <- boot.index <- matrix(nrow = n.obs, ncol = nsim)
    for(i in seq_len(nsim)) {
      boot.index[, i] <- sample(n.obs, replace = TRUE)
      boot.res[, i] <-
        getJitteredResiduals(object, jitter.scale = jitter.scale,
                             y = y[boot.index[, i]])
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
