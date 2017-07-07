#' @keywords internal
getResponseValues <- function(object, ...) {
  UseMethod("getResponseValues")
}


#' @keywords internal
getResponseValues.clm <- function(object, ...) {
  unname(as.integer(object$y))
}


#' @keywords internal
getResponseValues.polr <- function(object, ...) {
  # if (is.null(object$model)) {
  #   stop("Cannot extract response values from ", deparse(substitute(object)),
  #        ". Please re-fit ", deparse(substitute(object)),
  #        " using the option `model = TRUE`.")
  # } else {
  #   unname(as.integer(object$model$y))
  # }
  unname(as.integer(model.response(model.frame(object))))
}


#' @keywords internal
getResponseValues.vglm <- function(object, ...) {
  unname(apply(object@y, MARGIN = 1, FUN = function(x) which(x == 1)))
}
