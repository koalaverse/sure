#' @keywords internal
getDistributionFunction <- function(object) {
  UseMethod("getDistributionFunction")
}


#' @keywords internal
getDistributionFunction.clm <- function(object) {
  spec <- switch(object$link,
                 "logit" = qlogis,
                 "probit" = qnorm,
                 "loglog" = "gumbel",
                 "cloglog" = "Gumbel",
                 "cauchit" = qcauchy)
  if (!is.function(spec) && spec == "gumbel") {
    stop("The log-log link is not currently supported.")
  }
  if (!is.function(spec) && spec == "Gumbel") {
    stop("The complimentary log-log link is not currently supported.")
  }
  spec
}


#' @keywords internal
getDistributionFunction.polr <- function(object) {
  spec <- switch(object$method,
                 "logistic" = qlogis,
                 "probit" = qnorm,
                 "loglog" = "gumbel",
                 "cloglog" = "Gumbel",
                 "cauchit" = qcauchy)
  if (!is.function(spec) && spec == "gumbel") {
    stop("The log-log link is not currently supported.")
  }
  if (!is.function(spec) && spec == "Gumbel") {
    stop("The complimentary log-log link is not currently supported.")
  }
  spec
}


#' @keywords internal
getDistributionFunction.vglm <- function(object) {
  spec <- switch(object@family@infos()$link,
                 "logit" = qlogis,
                 "probit" = qnorm,
                 "loglog" = "gumbel",
                 "cloglog" = "Gumbel",
                 "cauchit" = qcauchy)
  if (!is.function(spec) && spec == "gumbel") {
    stop("The log-log link is not currently supported.")
  }
  if (!is.function(spec) && spec == "Gumbel") {
    stop("The complimentary log-log link is not currently supported.")
  }
  spec
}
