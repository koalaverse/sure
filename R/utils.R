# various internal functions

################################################################################
# The following functions have been taken from truncdist, but have been modified
# to not throw warnings when vectors are passed to arguments a and b
################################################################################

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


#' @keywords internal
sim_trunc <- function(n, distribution, a, b, location = 0, scale = 1) {
  if (distribution == "norm") {
    .rtrunc(n, spec = distribution, a = a, b = b,
            mean = location, sd = scale)
  } else {
    .rtrunc(n, spec = distribution, a = a, b = b,
            location = location, scale = scale)
  }
}


################################################################################
# Gumbel distribution functions
################################################################################

# For log-log link

#' @keywords internal
pgumbel <- function(q, location = 0, scale = 1) {
  q <- (q - location) / scale
  exp(-exp(-q))
}


#' @keywords internal
qgumbel <- function(p, location = 0, scale = 1) {
  -scale * log(-log(p)) + location
}


#' @keywords internal
rgumbel <- function (n, location = 0, scale = 1) {
  qgumbel(runif(n, min = 0, max = 1), location = location, scale = scale)
}


# For complimentary log-log link

#' @keywords internal
pGumbel <- function(q, location = 0, scale = 1) {
  q <- (q - location) / scale
  1 - exp(-exp(q))
}


#' @keywords internal
qGumbel <- function(p, location = 0, scale = 1) {
  scale * log(-log(1 - p)) + location
}


#' @keywords internal
rGumbel <- function (n, location = 0, scale = 1) {
  qGumbel(runif(n, min = 0, max = 1), location = location, scale = scale)
}


################################################################################
# Generic function to extract truncation bounds for cumulative link models;
# these are used when sampling the surrogate values
################################################################################

#' @keywords internal
getBounds <- function(object, ...) {
  UseMethod("getBounds")
}


#' @keywords internal
getBounds.clm <- function(object, ...) {
  unname(
    c(-Inf, stats::coef(object)[seq_len(ncat(object) - 1)] -
        stats::coef(object)[1L], Inf)
  )
}


#' @keywords internal
getBounds.glm <- function(object, ...) {
  y <- getResponseValues(object)
  c(ifelse(y == 0, yes = -Inf, no = 0), ifelse(y == 1, yes = Inf, no = 0))
}


#' @keywords internal
getBounds.lrm <- function(object, ...) {
  coefs <- -unname(stats::coef(object))
  c(-Inf, coefs[seq_len(ncat(object) - 1)] - coefs[1L], Inf)
}


#' @keywords internal
getBounds.orm <- function(object, ...) {
  coefs <- -unname(stats::coef(object))
  c(-Inf, coefs[seq_len(ncat(object) - 1)] - coefs[1L], Inf)
}


#' @keywords internal
getBounds.polr <- function(object, ...) {
  unname(
    c(-Inf, object$zeta - object$zeta[1L], Inf)
  )
}


#' @keywords internal
getBounds.vglm <- function(object, ...) {
  coefs <- if (object@misc$reverse) {
    -unname(stats::coef(object))
  } else {
    unname(stats::coef(object))
  }
  c(-Inf, coefs[seq_len(ncat(object) - 1)] - coefs[1L], Inf)
}


################################################################################
# Generic function for extracting the assumed cumulative distribution function
# from a cumulative link model
################################################################################

#' @keywords internal
getDistributionFunction <- function(object) {
  UseMethod("getDistributionFunction")
}


#' @keywords internal
getDistributionFunction.clm <- function(object) {
  switch(object$link,
         "logit" = plogis,
         "probit" = pnorm,
         "loglog" = pgumbel,
         "cloglog" = pGumbel,
         "cauchit" = pcauchy)
}


#' @keywords internal
getDistributionFunction.glm <- function(object) {
  switch(object$family$link,
         "logit" = plogis,
         "probit" = pnorm,
         # "loglog" = pgumbel,  # glm does not support this link function
         "cloglog" = pGumbel,
         "cauchit" = pcauchy)
}


#' @keywords internal
getDistributionFunction.lrm <- function(object) {
  plogis
}


#' @keywords internal
getDistributionFunction.orm <- function(object) {
  switch(object$family,
         "logistic" = plogis,
         "probit" = pnorm,
         "loglog" = pgumbel,
         "cloglog" = pGumbel,
         "cauchit" = pcauchy)
}


#' @keywords internal
getDistributionFunction.polr <- function(object) {
  switch(object$method,
         "logistic" = plogis,
         "probit" = pnorm,
         "loglog" = pgumbel,
         "cloglog" = pGumbel,
         "cauchit" = pcauchy)
}


#' @keywords internal
getDistributionFunction.vglm <- function(object) {
  switch(object@family@infos()$link,
         "logit" = plogis,
         "probit" = pnorm,
         "loglog" = pgumbel,
         "cloglog" = pGumbel,
         "cauchit" = pcauchy)
}


################################################################################
# Generic function for extracting the name of the assumed distribution from a
# cumulative link model
################################################################################

#' @keywords internal
getDistributionName <- function(object) {
  UseMethod("getDistributionName")
}


#' @keywords internal
getDistributionName.clm <- function(object) {
  switch(object$link,
         "logit" = "logis",
         "probit" = "norm",
         "loglog" = "gumbel",
         "cloglog" = "Gumbel",
         "cauchit" = "cauchy")
}


#' @keywords internal
getDistributionName.glm <- function(object) {
  switch(object$family$link,
         "logit" = "logis",
         "probit" = "norm",
         # "loglog" = "gumbel",  # glm does not support this link function
         "cloglog" = "Gumbel",
         "cauchit" = "cauchy")
}


#' @keywords internal
getDistributionName.lrm <- function(object) {
  "logis"
}


#' @keywords internal
getDistributionName.orm <- function(object) {
  switch(object$family,
         "logistic" = "logis",
         "probit" = "norm",
         "loglog" = "gumbel",
         "cloglog" = "Gumbel",
         "cauchit" = "cauchy")
}


#' @keywords internal
getDistributionName.polr <- function(object) {
  switch(object$method,
         "logistic" = "logis",
         "probit" = "norm",
         "loglog" = "gumbel",
         "cloglog" = "Gumbel",
         "cauchit" = "cauchy")
}


#' @keywords internal
getDistributionName.vglm <- function(object) {
  switch(object@family@infos()$link,
         "logit" = "logis",
         "probit" = "norm",
         "loglog" = "gumbel",
         "cloglog" = "Gumbel",
         "cauchit" = "cauchy")
}


################################################################################
# Generic function for extracting the fitted probabilities from a cumulative
# link model
################################################################################

#' @keywords internal
getFittedProbs <- function(object) {
  UseMethod("getFittedProbs")
}


#' @keywords internal
getFittedProbs.clm <- function(object) {
  newdata <- stats::model.frame(object)
  vars <- as.character(attr(object$terms, "variables"))
  resp <- vars[1 + attr(object$terms, "response")]  # response name
  newdata <- newdata[!names(newdata) %in% resp]
  predict(object, newdata = newdata, type = "prob")$fit
}


#' @keywords internal
getFittedProbs.glm <- function(object) {
  prob <- object$fitted.values
  cbind(prob, 1 - prob)
}


#' @keywords internal
getFittedProbs.lrm <- function(object) {
  predict(object, type = "fitted.ind")
}


#' @keywords internal
getFittedProbs.orm <- function(object) {
  predict(object, type = "fitted.ind")
}


#' @keywords internal
getFittedProbs.polr <- function(object) {
  object$fitted.values
}


#' @keywords internal
getFittedProbs.vglm <- function(object) {
  object@fitted.values
}


################################################################################
# Generic function for extracting the fitted mean response from a cumulative
# link model
################################################################################

#' @keywords internal
getMeanResponse <- function(object) {  # for j = 1
  UseMethod("getMeanResponse")
}


#' @keywords internal
getMeanResponse.clm <- function(object) {
  # Have to do this the long way, for now! :(
  mf <- model.frame(object)
  if (!is.null(cl <- attr(object$terms, "dataClasses"))) {
    .checkMFClasses(cl, mf)
  }
  X <- model.matrix(object$terms, data = mf, contrasts = object$contrasts)
  if(sum(object$aliased$beta) > 0) {
    X <- X[, !c(FALSE, object$aliased$beta), drop = FALSE]
  }
  # drop(X[, -1L, drop = FALSE] %*% object$beta)
  drop(X[, -1L, drop = FALSE] %*% object$beta - object$alpha[1L])
  # -predict(object, type = "linear.predictor")$eta2
}


#' @keywords internal
getMeanResponse.glm <- function(object) {
  object$linear.predictors
}


#' @keywords internal
getMeanResponse.lrm <- function(object) {
  # No negative sign since orm uses the reverse parameterization: Pr(Y >= j)
  predict(object, type = "lp", kint = 1L)
}


#' @keywords internal
getMeanResponse.orm <- function(object) {
  # No negative sign since orm uses the reverse parameterization: Pr(Y >= j)
  predict(object, type = "lp", kint = 1L)
}


#' @keywords internal
getMeanResponse.polr <- function(object) {
  # object$lp
  object$lp - object$zeta[1L]  # Xb - a1
}


#' @keywords internal
getMeanResponse.vglm <- function(object) {
  if (object@misc$reverse) {
    object@predictors[, 1L, drop = TRUE]
  } else {
    -object@predictors[, 1L, drop = TRUE]
  }
}


################################################################################
# Generic function for extracting the assumed quantile function from a
# cumulative link model
################################################################################

#' @keywords internal
getQuantileFunction <- function(object) {
  UseMethod("getQuantileFunction")
}


#' @keywords internal
getQuantileFunction.clm <- function(object) {
  switch(object$link,
         "logit" = qlogis,
         "probit" = qnorm,
         "loglog" = qgumbel,
         "cloglog" = qGumbel,
         "cauchit" = qcauchy)
}


#' @keywords internal
getQuantileFunction.glm <- function(object) {
  switch(object$family$link,
         "logit" = qlogis,
         "probit" = qnorm,
         "log" = qgumbel,
         "cloglog" = qGumbel,
         "cauchit" = qcauchy)
}


#' @keywords internal
getQuantileFunction.lrm <- function(object) {
  qlogis
}


#' @keywords internal
getQuantileFunction.orm <- function(object) {
  switch(object$family,
         "logistic" = qlogis,
         "probit" = qnorm,
         "loglog" = qgumbel,
         "cloglog" = qGumbel,
         "cauchit" = qcauchy)
}


#' @keywords internal
getQuantileFunction.polr <- function(object) {
  switch(object$method,
         "logistic" = qlogis,
         "probit" = qnorm,
         "loglog" = qgumbel,
         "cloglog" = qGumbel,
         "cauchit" = qcauchy)
}


#' @keywords internal
getQuantileFunction.vglm <- function(object) {
  switch(object@family@infos()$link,
         "logit" = qlogis,
         "probit" = qnorm,
         "loglog" = qgumbel,
         "cloglog" = qGumbel,
         "cauchit" = qcauchy)
}


################################################################################
# Generic function to extract the response values from a cumulative link or
# general model; returns an integer, not a factor!
################################################################################

#' @keywords internal
getResponseValues <- function(object, ...) {
  UseMethod("getResponseValues")
}


#' @keywords internal
getResponseValues.clm <- function(object, ...) {
  unname(as.integer(object$y))
}


#' @keywords internal
getResponseValues.glm <- function(object) {
  # FIXME: What about binomial models with matrix response, etc.?
  as.integer(as.factor(model.response(model.frame(object))))
}


#' @keywords internal
getResponseValues.lrm <- function(object) {
  as.integer(model.response(model.frame(object)))
}


#' @keywords internal
getResponseValues.orm <- function(object) {
  as.integer(model.response(model.frame(object)))
}


#' @keywords internal
getResponseValues.polr <- function(object, ...) {
  unname(as.integer(model.response(model.frame(object))))
}


#' @keywords internal
getResponseValues.vglm <- function(object, ...) {
  unname(apply(object@y, MARGIN = 1, FUN = function(x) which(x == 1)))
}


################################################################################
# Number of response categories
################################################################################

#' @keywords internal
ncat <- function(object) {
  UseMethod("ncat")
}


#' @keywords internal
ncat.clm <- function(object) {
  length(object$y.levels)
}


#' @keywords internal
ncat.glm <- function(object) {
  length(unique(getResponseValues(object)))
}


#' @keywords internal
ncat.lrm <- function(object) {
  object$non.slopes + 1
}


#' @keywords internal
ncat.orm <- function(object) {
  object$non.slopes + 1
}


#' @keywords internal
ncat.polr <- function(object) {
  length(object$lev)
}


#' @keywords internal
ncat.vglm <- function(object) {
  length(attributes(object)$extra$colnames.y)
}


################################################################################
# Surrogate and residual workhorse functions
################################################################################

#' @keywords internal
generate_surrogate <- function(object, method = c("latent", "jitter"),
                               jitter.scale = c("response", "probability"),
                               boot_id = NULL) {

  # Match arguments
  method <- match.arg(method)

  # Generate surrogate response values
  s <- if (method == "latent") {  # latent variable approach

    # Get distribution name (for sampling)
    distribution <- getDistributionName(object)  # distribution name

    # Simulate surrogate response values from the appropriate truncated
    # distribution
    if (distribution %in% c("norm", "logis", "cauchy", "gumbel", "Gumbel")) {
      y <- getResponseValues(object)
      if (is.null(boot_id)) {
        boot_id <- seq_along(y)
      }
      mean_response <- getMeanResponse(object)  # mean response values
      if (!inherits(object, what = "lrm") && inherits(object, what = "glm")) {
        sim_trunc(n = length(y), distribution = distribution,
                  # {0, 1} -> {1, 2}
                  a = ifelse(y[boot_id] == 1, yes = -Inf, no = 0),
                  b = ifelse(y[boot_id] == 2, yes =  Inf, no = 0),
                  location = mean_response[boot_id], scale = 1)  # surrogate values
      } else {
        trunc_bounds <- getBounds(object)  # truncation bounds
        sim_trunc(n = length(y), distribution = distribution,
                  a = trunc_bounds[y[boot_id]],
                  b = trunc_bounds[y[boot_id] + 1L],
                  location = mean_response[boot_id], scale = 1)  # surrogate values
      }
    } else {
      stop("Distribution not supported.", call. = FALSE)
    }

  } else {  # jittering approach

    # Determine scale for jittering
    jitter.scale <- match.arg(jitter.scale)
    y <- getResponseValues(object)
    if (is.null(boot_id)) {
      boot_id <- seq_along(y)
    }
    y <- y[boot_id]
    prob <- getFittedProbs(object)[boot_id, ]
    if (jitter.scale == "response") {  # jittering on the response scale
      j <- seq_len(ncol(prob))
      jmat <- matrix(rep(j, times = nrow(prob)), ncol = ncol(prob), byrow = TRUE)
      runif(length(y), min = y, max = y + 1)
    } else {  # jittering on the probability scale
      .min <- pbinom(y - 1, size = 1, prob = prob[, 1L, drop = TRUE])  # F(y-1)
      .max <- pbinom(y, size = 1, prob = prob[, 1L, drop = TRUE])  # F(y)
      runif(length(y), min = .min, max = .max)  # S|Y=y - E(S|X)
    }

  }

  # Return results
  s

}



#' @keywords internal
generate_residuals <- function(object, method = c("latent", "jitter"),
                               jitter.scale = c("response", "probability"),
                               boot_id = NULL) {

  # Match arguments
  method <- match.arg(method)

  # Generate surrogate response values
  r <- if (method == "latent") {  # latent variable approach

    # Get distribution name (for sampling)
    distribution <- getDistributionName(object)  # distribution name

    # Simulate surrogate response values from the appropriate truncated
    # distribution
    if (distribution %in% c("norm", "logis", "cauchy", "gumbel", "Gumbel")) {
      y <- getResponseValues(object)
      if (is.null(boot_id)) {
        boot_id <- seq_along(y)
      }
      mean_response <- getMeanResponse(object)  # mean response values
      s <-       if (!inherits(object, what = "lrm") &&
                     inherits(object, what = "glm")) {
        sim_trunc(n = length(y), distribution = distribution,
                  # {0, 1} -> {1, 2}
                  a = ifelse(y[boot_id] == 1, yes = -Inf, no = 0),
                  b = ifelse(y[boot_id] == 2, yes =  Inf, no = 0),
                  location = mean_response[boot_id], scale = 1)  # surrogate values
      } else {
        trunc_bounds <- getBounds(object)  # truncation bounds
        sim_trunc(n = length(y), distribution = distribution,
                  a = trunc_bounds[y[boot_id]],
                  b = trunc_bounds[y[boot_id] + 1L],
                  location = mean_response[boot_id], scale = 1)  # surrogate values
      }
    } else {
      stop("Distribution not supported.", call. = FALSE)
    }
    s - mean_response[boot_id]  # surrogate residuals
  } else {  # jittering approach
    jitter.scale <- match.arg(jitter.scale)
    y <- getResponseValues(object)
    if (is.null(boot_id)) {
      boot_id <- seq_along(y)
    }
    y <- y[boot_id]
    prob <- getFittedProbs(object)[boot_id, ]
    if (jitter.scale == "response") {  # jittering on the response scale
      j <- seq_len(ncol(prob))
      jmat <- matrix(rep(j, times = nrow(prob)), ncol = ncol(prob), byrow = TRUE)
      runif(length(y), min = y, max = y + 1) - rowSums((jmat + 0.5) * prob)
    } else {  # jittering on the probability scale
      .min <- pbinom(y - 1, size = 1, prob = prob[, 1L, drop = TRUE])  # F(y-1)
      .max <- pbinom(y, size = 1, prob = prob[, 1L, drop = TRUE])  # F(y)
      runif(length(y), min = .min, max = .max) - 0.5  # S|Y=y - E(S|X)
    }
  }

  # Return results
  r

}
