#' Surrogate Residuals
#'
#' Surrogate-based residuals for cumulative link and general regression models.
#'
#' @param object An object of class \code{\link[ordinal]{clm}},
#' \code{\link[stats]{glm}}, \code{\link[rms]{lrm}}, \code{\link[rms]{orm}},
#' \code{\link[MASS]{polr}}, or \code{\link[VGAM]{vglm}}.
#'
#' @param method Character string specifying the type of surrogate to use; for
#' details, see Liu and Zhang (2017). For cumulative link models, the latent
#' variable method is used. For binary GLMs, the jittering approach is employed.
#' (Currently ignored.)
#'
#' @param jitter.scale Character string specifying the scale on which to perform
#' the jittering. Should be one of \code{"probability"} or \code{"response"}.
#' (Currently ignored for cumulative link models.)
#'
#' @param nsim Integer specifying the number of bootstrap replicates to use.
#' Default is \code{1L} meaning no bootstrap samples.
#'
#' @param ... Additional optional arguments. (Currently ignored.)
#'
#' @return A numeric vector of class \code{c("numeric", "resid")} containing the
#' residuals. Additionally, if \code{nsim} > 1, then the result will contain the
#' attributes:
#' \describe{
#'   \item{\code{boot.reps}}{A matrix  with \code{nsim} columns, one for each
#'   bootstrap replicate of the residuals. Note, these are random and do not
#'   correspond to the original ordering of the data;}
#'   \item{\code{boot.id}}{A matrix  with \code{nsim} columns. Each column
#'   contains the observation number each residual corresponds to in
#'   \code{boot.reps}. (This is used for plotting purposes.)}
#' }
#'
#' @note Surrogate residuals require sampling from a continuous distribution;
#' consequently, the result will be different with every call to \code{resids}.
#' The internal functions used for sampling from truncated distributions when
#' \code{method = "latent"} are based on modified versions of
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
#'
#' @examples
#' #
#' # Residuals for binary GLMs using the jittering method
#' #
#'
#' # Simulate logistic regression data with quadratic trend
#' set.seed(101)  # for reproducibility
#' n <- 1000
#' x <- runif(n, min = 1, max = 7)
#' y <- rbinom(n, size = 1, prob = plogis(16 - 8 * x + 1 * x ^ 2))
#' d <- data.frame("x" = x, "y" = as.factor(y))
#'
#' # Fit logistic regression models (with and without quadratic trend)
#' fit1 <- glm(y ~ x + I(x ^ 2), data = d, family = binomial)  # correct model
#' fit2 <- glm(y ~ x, data = d, family = binomial)  # missing quadratic trend
#'
#' # Construct residuals
#' set.seed(102)  # for reproducibility
#' res1 <- resids(fit1)
#' res2 <- resids(fit2)
#'
#' # Residual-vs-covariate plots
#' par(mfrow = c(1, 2))
#' scatter.smooth(d$x, res1, lpars = list(lwd = 2, col = "red"),
#'                xlab = expression(x), ylab = "Surrogate residual",
#'                main = "Correct model")
#' scatter.smooth(d$x, res2, lpars = list(lwd = 2, col = "red"),
#'                xlab = expression(x), ylab = "Surrogate residual",
#'                main = "Incorrect model")
#'
#' \dontrun{
#' #
#' # Residuals for cumulative link models using the latent method
#' #
#'
#' # Load required packages
#' library(ggplot2)  # for autoplot function
#' library(MASS)     # for polr function
#' library(ordinal)  # for clm function
#'
#' #
#' # Detecting a misspecified mean structure
#' #
#'
#' # Data simulated from a probit model with a quadratic trend
#' data(df1)
#' ?df1
#'
#' # Fit a probit model with/without a quadratic trend
#' fit.bad <- polr(y ~ x, data = df1, method = "probit")
#' fit.good <- polr(y ~ x + I(x ^ 2), data = df1, method = "probit")
#'
#' # Some residual plots
#' p1 <- autoplot(fit.bad, what = "covariate", x = df1$x)
#' p2 <- autoplot(fit.bad, what = "qq")
#' p3 <- autoplot(fit.good, what = "covariate", x = df1$x)
#' p4 <- autoplot(fit.good, what = "qq")
#'
#' # Display all four plots together (top row corresponds to bad model)
#' grid.arrange(p1, p2, p3, p4, ncol = 2)
#'
#' #
#' # Detecting heteroscedasticity
#' #
#'
#' # Data simulated from a probit model with heteroscedasticity.
#' data(df2)
#' ?df2
#'
#' # Fit a probit model with/without a quadratic trend
#' fit <- polr(y ~ x, data = df2, method = "probit")
#'
#' # Some residual plots
#' p1 <- autoplot(fit, what = "covariate", x = df1$x)
#' p2 <- autoplot(fit, what = "qq")
#' p3 <- autoplot(fit, what = "fitted")
#'
#' # Display all three plots together
#' grid.arrange(p1, p2, p3, ncol = 3)
#'
#' #
#' # Detecting a misspecified link function
#' #
#'
#' # Data simulated from a log-log model with a quadratic trend.
#' data(df3)
#' ?df3
#'
#' # Fit models with correctly specified link function
#' clm.loglog <- clm(y ~ x + I(x ^ 2), data = df3, link = "loglog")
#' polr.loglog <- polr(y ~ x + I(x ^ 2), data = df3, method = "loglog")
#'
#' # Fit models with misspecified link function
#' clm.probit <- clm(y ~ x + I(x ^ 2), data = df3, link = "probit")
#' polr.probit <- polr(y ~ x + I(x ^ 2), data = df3, method = "probit")
#'
#' # Q-Q plots of the residuals (with bootstrapping)
#' p1 <- autoplot(clm.probit, nsim = 50, what = "qq") +
#'   ggtitle("clm: probit link")
#' p2 <- autoplot(clm.loglog, nsim = 50, what = "qq") +
#'   ggtitle("clm: log-log link (correct link function)")
#' p3 <- autoplot(polr.probit, nsim = 50, what = "qq") +
#'   ggtitle("polr: probit link")
#' p4 <- autoplot(polr.loglog, nsim = 50, what = "qq") +
#'   ggtitle("polr: log-log link (correct link function)")
#' grid.arrange(p1, p2, p3, p4, ncol = 2)
#'
#' # We can also try various goodness-of-fit tests
#' par(mfrow = c(1, 2))
#' plot(gof(clm.probit, nsim = 50))
#' plot(gof(clm.loglog, nsim = 50))
#' }
resids <- function(object, ...) {
  UseMethod("resids")
}


#' @rdname resids
#' @export
resids.default <- function(object, method = c("latent", "jitter"),
                           jitter.scale = c("probability", "response"),
                           nsim = 1L, ...) {

  # Sanity check
  if (!inherits(object, c("clm", "glm", "lrm", "orm", "polr", "vglm"))) {
    stop(deparse(substitute(object)), " should be of class \"clm\", \"glm\", ",
         "\"lrm\", \"orm\", \"polr\", or \"vglm\".")
  }

  # method <- match.arg(method)
  # jitter.scale <- match.arg(jitter.scale)
  #
  # y <- getResponseValues(object)
  # if (method == "latent") {
  #
  # } else {  # jittering
  #
  #   # Construct residuals
  #   res <- getJitteredResiduals(object, jitter.scale = jitter.scale, y = y)
  #
  #   # Multiple samples
  #
  # }

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
resids.lrm <- function(object, method = c("latent", "jitter"),
                       jitter.scale = c("probability", "response"), nsim = 1L,
                       ...) {
  resids.default(object, nsim = nsim, ...)
}


#' @rdname resids
#' @export
resids.glm <- function(object, jitter.scale = c("probability", "response"),
                       nsim = 1L, ...) {

  # Print warning message
  warning("Using sure with \"glm\" objects is still experimental. Use with ",
          "caution.")

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
