#' @importFrom pracma Toeplitz

# chow: Singular Toeplitz lower Hessenberg matrix -------------------------------

#' @name chow
#' @title Creating singular Toeplitz lower Hessenberg matrix
#'
#' @description returns matrix \code{A = H(alpha) + delta * EYE}, such that
#'   \code{H[i,j] = alpha^(i-j+1)}.
#'
#' @param n order of the matrix
#' @param alpha defaults to 1
#' @param delta defaults to 0
#'
#' @export
chow <- function(n, alpha = 1, delta = 0){
  row <- alpha^(1:n)
  col <- c(alpha, 1, rep(0,n-2))
  A <- Toeplitz(row, col) + delta * diag(n)

  return(A)
}
