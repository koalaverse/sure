#' @keywords internal
nobs.vglm <- function(object, ...) {
  attributes(object)$misc$n
}


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
  if (is.null(object$model)) {
    stop("Cannot extract response values from ", deparse(substitute(object)),
         ". Please re-fit ", deparse(substitute(object)),
         " using the option `model = TRUE`.")
  } else {
    unname(as.integer(object$model$y))
  }
}


#' @keywords internal
getResponseValues.vglm <- function(object, ...) {
  unname(apply(object@y, MARGIN = 1, FUN = function(x) which(x == 1)))
}


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
getBounds.polr <- function(object, ...) {
  unname(
    c(-Inf, object$zeta - object$zeta[1L], Inf)
  )
}


#' @keywords internal
getBounds.vglm <- function(object, ...) {
  unname(
    c(-Inf, stats::coef(object)[seq_len(ncat(object) - 1)] -
        stats::coef(object)[1L], Inf)
  )
}


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
  -(object$alpha[1L] - X[, -1L, drop = FALSE] %*% object$beta)[,, drop = TRUE]
  # -predict(object, type = "linear.predictor")$eta2
}


#' @keywords internal
getMeanResponse.polr <- function(object) {
  dist.fun <- getDistributionFunction(object)
  -dist.fun(object$fitted.values[, 1L, drop = TRUE])
}


#' @keywords internal
getMeanResponse.vglm <- function(object) {
  -object@predictors[, 1L, drop = TRUE]  # FIXME: Why the minus sign?
}
