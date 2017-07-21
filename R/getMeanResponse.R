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
  drop(X[, -1L, drop = FALSE] %*% object$beta - object$alpha[1L])
  # -predict(object, type = "linear.predictor")$eta2
}


#' @keywords internal
getMeanResponse.lrm <- function(object) {
  object$linear.predictors - object$coefficients[1L]
}


#' @keywords internal
getMeanResponse.orm <- function(object) {
  medy <- quantile(getResponseValues(object), probs = 0.5, type = 1L)
  kmid <- max(1, which(1L:length(object$yunique) == medy) - 1L)
  object$linear.predictors - object$coefficients[kmid]
}


#' @keywords internal
getMeanResponse.polr <- function(object) {
  object$lp - object$zeta[1L]  # Xb - a1
}


#' @keywords internal
getMeanResponse.vglm <- function(object) {
  -object@predictors[, 1L, drop = TRUE]  # FIXME: Why the minus sign?
}
