#' Arrange multiple grobs on a page
#'
#' See \code{\link[gridExtra]{grid.arrange}} for more details.
#'
#' @name grid.arrange
#' @rdname grid.arrange
#' @keywords internal
#' @export
#' @importFrom gridExtra grid.arrange
#' @usage grid.arrange(..., newpage = TRUE)
NULL


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
