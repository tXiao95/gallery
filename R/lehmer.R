# lehmer: Lehmer matrix -----------------------------------------------------------

#' @name lehmer
#' @title Create Lehmer matrix
#'
#' @description the symmetric positive-definite matrix such that A[i,j] = i/j, for j >= i
#'
#' @param n order of matrix
#'
#' @export
lehmer <- function(n){
  A <- matrix(0, n, n)
  i <- row(A)
  j <- col(A)
  I <- i / j
  J <- j / i
  A[j >= i] <- I[j >= i]
  A[j < i] <- J[j < i]

  return(A)
}
