# tridiag: Sparse tridiagonal matrix -----------------------------------------------

#' @name tridiag
#' @title Create sparse tridiagonal matrix
#'
#' @description Create a sparse tridiagonal matrix of dgcMatrix class.
#'
#' @param n dimension of the square matrix
#' @param x subdiagonal (-1)
#' @param y diagonal (0)
#' @param z superdiagonal (+1)
#'
#' @return Sparse tridiagonal matrix
#'
#' @export
tridiag <- function(n, x=NULL, y=NULL, z=NULL){
  if(is.null(x)){
    x <- -1; y <- 2; z <- -1
  }
  if(is.null(z)){
    z <- y; y <- x; x <- n
  }

  if(max(c(length(x), length(y), length(z))) == 1){
    x <- x * rep(1, n-1)
    z <- z * rep(1, n-1)
    y <- y * rep(1, n)
  } else{
    nx <- length(x)
    ny <- length(y)
    nz <- length(z)
    if((ny - nx - 1) | (ny - nz - 1)){
      stop("tridiag:InvalidVectorArgDim")
    }
  }
  n <- length(y)

  return(spdiags(matrix(c(x, 0, y, 0, z), nrow = n), -1:1, n, n))
}
