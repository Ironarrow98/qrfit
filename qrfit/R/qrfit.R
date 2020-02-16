#' The Fitting function of conditional quantile regression model
#'
#' Solve for `beta_hat` of the minimization problem of the objective function of conditional quantile regression model.
#'
#' @param y The (n * 1) dependent response variable vector of the regression model.
#' @param X The (n * p) independent cause variable matrix of the regression model.
#' @param tau The constant level chosen according to which quantile needs to be estimated.
#' @return The value of `beta_hat` that minimize the objective function of conditional quantile regression model.
#' @export
qr_fit <- function(y, X, tau) {
  n <- nrow(X)
  p <- ncol(X)
  a <- c(rep(0, 2 * p), tau * rep(1, n), (1 - tau) * rep(1, n))
  B <- cbind(X, -X, diag(n), -diag(n))
  const <- rep("=", n)
  lprog <- lp("min", a, B, const, y)
  beta_h <- lprog$solution[1:p] -  lprog$solution[((p + 1):(2 * p))]
  return(beta_h)
}


