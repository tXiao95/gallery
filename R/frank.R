#' @importFrom Matrix triu

# frank matrix ---------------------------------------------------------

#' @name frank
#' @title Frank matrix of order \code{N}
#'
#' @description Frank matrix of order \code{N}. It is upper Hessenberg with
#' determinant 1.
#'
#' @param n order of the matrix
#' @param k If k is 1, the elements are reflected about the anti-diagonal.
#'
#' @export
frank <- function(n, k = 0){
  F <- minij(n)
  F <- as.matrix(triu(F, -1))

  if(k == 0){
    p <- n:1
    F <- t(F[p, p])
  }
  return(F)
}
