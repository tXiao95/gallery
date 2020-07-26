# chebvand: Vandermonde-like matrix for Chebyshev polynomials ---------------------------------------------------------

#' @name chebvand
#' @title Creating Vandermonde-like matrix for the Chebyshev polynomials
#'
#' @description Produces the (primal) Chebyshev Vandermonde matrix based on
#'   the points \code{p}. \code{C[i,j] = T_{i-1}p[j]}, where \code{T_{i-1}} is the Chebyshev
#'   polynomial of degree \code{i-1}
#'
#' @param p points to evaluate. If a scalar, then \code{p} equally spaced points
#'   on \code{[0,1]} are used.
#' @param m number of rows of the matrix. \code{chebvand(p, m)} is the rectangular version of
#'   \code{chebvand(p)} with \code{m} rows.
#'
#' @export
chebvand <- function(p, m = NULL){
  # If no rectangular row argument, make square matrix. Else, rectangular and m != n
  if(is.null(m)){
    #m <- p
    square <- 1
  } else{
    square <- 0
  }
  n <- length(p)

  # If p is scalar, make ncols = p and change p to vector within [0,1] of length p
  if(n == 1){
    n <- p
    p <- seq(0, 1, length.out = n)
  }

  # if will be square matrix, make nrows = ncols
  if(square == 1){
    m <- n
  }

  # make p a row vector
  p <- matrix(p, ncol = length(p))
  # Create m x n matrix, and since first row will be all 1's.
  C <- matrix(1, nrow = m, ncol = n)
  if(m == 1){
    return(C)
  }
  C[2,] <- p
  if(m == 2){
    return(C)
  }
  for(i in 3:m){
    C[i,] <- 2*p*C[i-1,] - C[i-2,]
  }

  return(C)
}
