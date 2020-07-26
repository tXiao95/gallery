#' @importFrom pracma pascal

# binomial_matrix: binomial matrix ---------------------------------------------------------

#' @name binomial_matrix
#' @title Create binomial matrix
#'
#' @description Binomial matrix: an N-by-N multiple of an involuntory matrix with
#' integer entries such that $A^2 = 2^(N-1)*I_N$
#' Thus B = A*2^((1-N)/2) is involutory, that is B^2 = EYE(N)
#'
#' @param n - row dimension
#'
#' @export
binomial_matrix <- function(n){
  L <- abs(pascal(n, 1))
  U <- L[n:1,n:1]
  D <- diag((-2)^(0:(n-1)))
  
  return(L %*% D %*% U)
}