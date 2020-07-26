# chebspec: Chebyshev spectral differentiation matrix ---------------------------------------------------------

#' @name chebspec
#' @title Create Chebyshev spectral differentiation matrix
#'
#' @description Chebyshev spectral differentiation matrix of order \code{n}. \code{k} determines
#'   the character of the output matrix. For either form, the eigenvector matrix is ill-conditioned.
#'
#' @param n order of the matrix.
#' @param k \code{k=0} is the default, no boundary conditions. The matrix is similar to a Jordan block
#'   of size \code{n} with eigenvalue 0. If \code{k=1}, the matrix is nonsingular
#'   and well-conditioned, and its eigenvalues have negative real parts.
#'
#' @export
chebspec <- function(n, k = NULL){
  if(is.null(k)){
    k <- 0
  }
  if(k == 1){
    n <- n + 1
  }
  if(!(k %in% 0:1)){
    stop("k must be 0 or 1")
  }

  n <- n - 1
  C <- matrix(0, nrow = n + 1, ncol = n + 1)

  x <- cos(matrix(0:n, ncol = 1) * (pi/n))
  d <- matrix(1, nrow = n + 1, ncol = 1)
  d[1] <- 2; d[n + 1] <- 2

  X <- matrix(x, nrow = n + 1, ncol = n + 1) - matrix(x, nrow = n + 1, ncol = n + 1, byrow = T)
  C <- (d %*% t(matrix(1, nrow = n + 1, ncol = 1) / d)) / (X + diag(nrow(C)))

  # Now fix diagonal and signs
  C[1,1] <- (2*n^2 + 1) / 6
  for(i in 2:(n + 1)){
    if((i %% 2) == 0){
      C[,i] <- -C[,i]
      C[i,] <- -C[i,]
    }
    if(i < (n+1)){
      C[i,i] <- -x[i] / (2 * (1 - x[i]^2))
    } else{
      C[n + 1,n + 1] <- -C[1,1]
    }
  }

  if(k == 1){
    C <- C[2:(n + 1),2:(n + 1)]
  }

  return(C)
}
