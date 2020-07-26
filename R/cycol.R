#' @importFrom stats rnorm

# cycol: Matrix whose columns repeat cyclically-------------------------------------

#' @name cycol
#' @title Create matrix \code{A} whose columns repeat cyclically
#'
#' @description Returns an \code{n}-by-\code{n} matrix with cyclically repeating columns
#'   where one cycle consists of the columns defined by \code{randn(n,k)}. Thus, the
#'   rank of matrix \code{A} cannot exceed \code{k}, and \code{k} must be scalar.
#'
#' @param n number of columns of matrix
#' @param k upper limit of rank
#' @param m number of rows of matrix
#'
#' @export
cycol <- function(n, k, m=NULL){
  if(is.null(m)){m <- n}
  nums <- rnorm(m*k)
  return(suppressWarnings(matrix(nums, nrow=m, ncol=n)))
}
