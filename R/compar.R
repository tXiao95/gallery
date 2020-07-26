# compar: Comparison matrices ---------------------------------------------

#' @name compar
#' @title Create comparison matrix \code{A}
#'
#' @description For \code{k=0}, if \code{i==j}, $A[i,j]=abs(B[i,j])$ and
#'   \code{A[i,j]=-abs(B[i,j])} otherwise. For \code{k=1}, \code{A} replaces each
#'   diagonal element of \code{B} with its absolute value, and replaces each
#'   off-diagonal with the negative of the largest absolute value off-diagonal
#'   in the same row.
#'
#' @param B input matrix
#' @param k decides what matrix to return
#'
#' @export
compar <- function(B, k = 0){
  m <- nrow(B)
  n <- ncol(B)

  d <- abs(diag(B))
  if(k == 0){
    A <- -abs(B)
  } else if(k == 1){
    diag(B) <- 0
    for(i in 1:nrow(A)){
      row <- A[i,]
      val <- -max(abs(row))
      A[i,] <- val
    }
  }
  diag(A) <- d

  return(A)
}
