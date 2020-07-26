#' @importFrom Matrix triu tril

# grcar: Toeplitz matrix with sensitive eigenvalues ------------------------------

#' @name grcar
#' @title Create Toeplitz matrix with sensitive eigenvalues
#'
#' @description Eigenvalues are sensitive.
#'
#' @param n dimension of the square matrix
#' @param k number of superdiagonals of ones
#'
#' @return \code{n}-by-\code{n} Toeplitz matrix with -1 on subdiagonal, 1 on diagonal, and \code{k} superdiagionals of 1s.
#'
#' @export
grcar <- function(n, k=NULL){
  if(is.null(k)){
    k <- 3
  }
  A <- tril(triu(matrix(1, nrow = n, ncol = n)), k)
  i <- row(A)
  j <- col(A)
  A[i == j + 1] <- -1

  return(as.matrix(A))
}
