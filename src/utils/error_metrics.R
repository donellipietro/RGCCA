#' Compute the Euclidean norm of a vector or matrix.
#'
#' @param x Input object.
#' @return The value produced by `norm_l2`.
norm_l2 <- function(x) {
  return(sqrt(as.numeric(t(x) %*% x)))
}
#' Compute a norm under a matrix-induced inner product.
#'
#' @param x Input object.
#' @param A Matrix defining the inner product.
#' @return The value produced by `norm_A`.
norm_A <- function(x, A) {
  return(sqrt(as.numeric(t(x) %*% A %*% x)))
}
#' Compute the root mean squared error over all entries of an object.
#'
#' @param x Input object.
#' @return The value produced by `RMSE`.
RMSE <- function(x) {
  ## Coerce to matrix
  x <- as.matrix(x)
  ## Extract dimensions
  n_stat_unit <- ncol(x)
  n_locs <- nrow(x)
  ## Return RMSE
  return(sqrt(sum(x^2) / (n_stat_unit * n_locs)))
}
#' Compute the integrated root mean squared error using a mass matrix.
#'
#' @param x Input object.
#' @param R0 Mass matrix used for integration.
#' @return The value produced by `IRMSE`.
IRMSE <- function(x, R0) {
  ## Coerce to matrix
  x <- as.matrix(x)
  ## Extract number of statistical units
  n_stat_unit <- ncol(x)
  ## Return integrated RMSE
  return(sqrt(sum(diag(t(x) %*% R0 %*% x)) / n_stat_unit))
}
#' Compute the angle between two discretized functions under a mass matrix.
#'
#' @param f1 First discretized function.
#' @param f2 Second discretized function.
#' @param R0 Mass matrix used for integration.
#' @return The value produced by `angle_between_functions`.
angle_between_functions <- function(f1, f2, R0) {
  ## Compute norms under R0
  norm_f1 <- sqrt(as.numeric(t(f1) %*% R0 %*% f1))
  norm_f2 <- sqrt(as.numeric(t(f2) %*% R0 %*% f2))
  ## Compute inner product
  f1_dot_f2 <- as.numeric(t(f1) %*% R0 %*% f2)
  ## Return angle in radians
  return(acos(f1_dot_f2 / (norm_f1 * norm_f2)))
}