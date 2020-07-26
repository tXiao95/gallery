#' @importFrom pracma Toeplitz

# circul: Circulant matrix --------------------------------------------------------

#' @name circul
#' @title Create circulant matrix
#'
#' @description Each row is obtained from the previous by cyclically permuting the
#'   entries one step forward. A special Toeplitz matrix in which diagonals "wrap around"
#'
#' @param v first row of the matrix. If \code{v} is a scalar, then \code{C = circul(1:v)}
#'
#' @return a circulant matrix whose first row is the vector \code{v}
#'
#' @export
circul <- function(v){
  if(length(v) == 1){
    v <- 1:v
  }
  n <- length(v)
  A <- Toeplitz(c(v[1], v[n:2]), v)

  return(A)
}
