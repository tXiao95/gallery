#' @importFrom pracma Diag

# clement: Tridiagonal matrix with zero diagonal entries ------------------

#' @name clement
#' @title Create Clement tridiagonal matrix with zero diagonal entries
#'
#' @description Returns an \code{n}-by-\code{n} tridiagonal matrix with zeros on the
#'   main diagonal. For \code{k=0}, \code{A} is nonsymmetric. For \code{k=1, A} is
#'   symmetric
#'
#' @param n order of matrix
#' @param k 0 indicates symmetric matrix, 1 asymmetric
#'
#' @export
clement <- function(n, k = 0){
  n <- n - 1
  z <- 1:n
  x <- n:1

  if(k == 0){
    A <- Diag(x, -1) + Diag(z, 1)
  } else{
    y <- sqrt(x * z)
    A <- Diag(y, -1) + Diag(y, 1)
  }

  return(A)
}
