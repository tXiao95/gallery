# cauchy_matrix: Cauchy matrix ------------------------------------------------------------------

#' @name cauchy_matrix
#' @title Create Cauchy matrix
#'
#' @description Arguments \code{x} and \code{y} are vectors of length \code{n}.
#'   \code{C[i,j] = 1 / (x[i] + y[j])}
#'
#' @param x vector of length n
#' @param y vector of length n
#'
#' @export
cauchy_matrix <- function(x,y=NULL){
  n <- length(x)
  if(n == 1){
    n <- x
    x <- 1:n
  }
  if(is.null(y)){
    y <- x
  }
  
  if(length(x) != length(y)){
    stop("cauchy:ParamLengthMismatch")
  }
  
  return(1 / (matrix(x, nrow = n, ncol = n) + matrix(y, nrow = n, ncol = n, byrow = T)))
}