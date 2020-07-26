#' @importFrom Matrix sparseMatrix

# lauchli: Lauchli matrix ----------------------------------------------------------

#' @name lauchli
#' @title Create Lauchli Matrix
#'
#' @description the (N + 1) x (N) matrix [ones(1,n); mu*eye(n)]. Well-known example in least squares of the
#'   danger of forming t(A) %*% A (due to inexact arithmetic, gives singular matrix)
#'
#' @param n number of columns
#' @param mu constant applied to identity
#' @param sparse whether matrix should be sparse
#'
#' @return Lauchli matrix.
#'
#' @export
lauchli <- function(n, mu = NULL, sparse = F){
  if(is.null(mu)){
    mu <- sqrt(.Machine$double.eps)
  }
  if(sparse){
    rows <- c(rep(1, n), 2:(n + 1))
    cols <- c(1:n, 1:n)
    A <- sparseMatrix(i = rows, j = cols, x = c(rep(1, n), rep(mu, length(rows) - n)))
  } else{
    A <- mu * diag(n)
    A <- rbind(rep(1, n), A)
  }

  return(A)
}
