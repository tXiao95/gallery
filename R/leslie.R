#' @importFrom pracma Diag
#' @importFrom Matrix sparseMatrix

# leslie: Leslie matrix -----------------------------------------------------------

#' @name leslie
#' @title Create Leslie population model matrix
#'
#' @description N by N matrix from Leslie population model with average birth and survival rates.
#'
#' @param a average birth numbers (first row)
#' @param b survival rates (subdiagonal)
#' @param sparse whether to return a sparse matrix
#'
#' @return N by N Leslie population model matrix
#'
#' @export
leslie <- function(a, b=NULL, sparse = F){
  if(is.null(b)){
    n <- a
    a <- rep(1, n)
    b <- rep(1, n-1)
  }
  if(length(a) != length(b) + 1){
    stop("a must have length = length(b) + 1")
  }
  if(sparse){
    n <- length(a)
    i <- c(rep(1, n), 2:n)
    j <- c(1:n, 1:(n-1))
    L <- sparseMatrix(i = i, j = j, x = c(a, b))
  } else{
    L <- Diag(b, -1)
    L[1,] <- a
  }

  return(L)
}
