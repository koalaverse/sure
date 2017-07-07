#' @keywords internal
ncat <- function(object, ...) {
  UseMethod("ncat")
}


#' @keywords internal
ncat.clm <- function(object, ...) {
  length(object$y.levels)
}


#' @keywords internal
ncat.polr <- function(object, ...) {
  length(object$lev)
}


#' @keywords internal
ncat.vglm <- function(object, ...) {
  length(attributes(object)$extra$colnames.y)
}
