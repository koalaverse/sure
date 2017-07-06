#' @keywords internal
getDistributionName <- function(object) {
  UseMethod("getDistributionName")
}


#' @keywords internal
getDistributionName.clm <- function(object) {
  spec <- switch(object$link,
                 "logit" = "logis",
                 "probit" = "norm",
                 "loglog" = "gumbel",
                 "cloglog" = "Gumbel",
                 "cauchit" = "cauchy")
  if (spec == "gumbel") {
    stop("The log-log link is not currently supported.")
  }
  if (spec == "Gumbel") {
    stop("The complimentary log-log link is not currently supported.")
  }
  spec
}


#' @keywords internal
getDistributionName.polr <- function(object) {
  spec <- switch(object$method,
                 "logistic" = "logis",
                 "probit" = "norm",
                 "loglog" = "gumbel",
                 "cloglog" = "Gumbel",
                 "cauchit" = "cauchy")
  if (spec == "gumbel") {
    stop("The log-log link is not currently supported.")
  }
  if (spec == "Gumbel") {
    stop("The complimentary log-log link is not currently supported.")
  }
  spec
}


#' @keywords internal
getDistributionName.vglm <- function(object) {
  spec <- switch(object@family@infos()$link,
                 "logit" = "logis",
                 "probit" = "norm",
                 "loglog" = "gumbel",
                 "cloglog" = "Gumbel",
                 "cauchit" = "cauchy")
  if (spec == "gumbel") {
    stop("The log-log link is not currently supported.")
  }
  if (spec == "Gumbel") {
    stop("The complimentary log-log link is not currently supported.")
  }
  spec
}
