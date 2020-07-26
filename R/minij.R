# SPD matrix min(i,j) ---------------------------------------------------------

#' @name minij
#' @title Symmetric positive definite matrix \code{MIN(i,j)}
#'
#' @description The \code{N}-by-\code{N} SPD matrix with \code{A[i,j]=min(i,j)}
#'
#' @param n order of the matrix
#'
#' @export
minij <- function(n){
  A <- matrix(0, n, n)
  pmin(row(A), col(A))
}
