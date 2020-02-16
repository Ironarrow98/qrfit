#' The loss function of conditional quantile regression model
#'
#' Calculates `z*(tau-1{z<0})`.
#'
#' @param z The value of the objective function needed to be passed to the loss function.
#' @param tau The constant level chosen according to which quantile needs to be estimated.
#' @return The value of the loss function with indicator function  I{z < 0}.
#' @export
qr_loss <- function(z, tau) {
  i <- ifelse(z < 0, 1, 0)
  return(z * (tau - i))
}

