#' The objective function of conditional quantile regression model
#'
#' @param beta The (p * 1) coefficient vector of the regression model.
#' @param y The (n * 1) dependent response variable vector of the regression model.
#' @param X The (n * p) independent cause variable matrix of the regression model.
#' @param tau The constant level chosen according to which quantile needs to be estimated.
#' @return The value of the objective function of conditional quantile regression model.
#' @export
qr_obj <- function(beta, y, X, tau) {
  z <- y - c(X %*% beta)
  result <- sum(qr_loss(z, tau))
  return(result)
}

