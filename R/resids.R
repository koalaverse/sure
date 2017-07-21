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
#'
#' @examples
#' \dontrun{
#' # Load required packages
#' library(ggplot2)
#' library(MASS)
#' library(ordr)
#'
#' # Fit a proportional odds model
#' house.polr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing,
#'                    method = "logistic")
#'
#' # Residuals
#' set.seed(101)  # for reproducibility
#' res <- resids(house.polr)
#' autoplot(res, what = "qq", distribution = qlogis)
#'
#' # Bootstrap residuals
#' set.seed(101)  # for reproducibility
#' res.boot <- resids(house.polr, nsim = 100)
#' autoplot(res.boot, what = "qq", distribution = qlogis)
#'
#' # Can also plot the residuals directly from the model
#' set.seed(101)  # for reproducibility
#' p1 <- autoplot(house.polr, nsim = 100, what = "qq")  # no need to supply dist
#' p2 <- autoplot(house.polr, nsim = 100, what = "fitted")
#' grid.arrange(p1, p2, ncol = 2)
#' }
resids <- function(object, type, jitter.scale, nsim, ...) {
  UseMethod("resids")
}


#' @rdname resids
#' @export
resids.default <- function(object, type = c("surrogate", "jitter"),
                           jitter.scale = c("response", "probability"),
                           nsim = 1, ...) {

  # Sanity check
  if (!inherits(object, c("clm", "lrm", "orm", "polr"))) {
    stop(deparse(substitute(object)), " should be of class \"clm\", \"polr\", ",
         "\"orm\", or \"vglm\".")
  }

  # Extract number of observations, response values, and truncation bounds
  y <- getResponseValues(object)
  n.obs <- length(y)
  bounds <- getBounds(object)

  # What type of residuals?
  type <- match.arg(type)
  # jitter.scale <- match.arg(jitter.scale)
  if (type == "jitter") {  # throw error, for now!
    stop("Jittering technique is not yet implemented.")
  }

  # Construct residuals
  mr <- getMeanResponse(object)  # -f(x; beta)
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
