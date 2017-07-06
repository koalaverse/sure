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
