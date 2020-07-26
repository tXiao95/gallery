#' @importFrom pracma Diag

# jordbloc: Jordan block matrix -------------------------------------------
#'
#' @name jordbloc
#' @title Create Jordan block matrix
#'
#' @description Returns a \code{n}-by-\code{n} JOrdan block with eigenvalue
#'   \code{lambda}. The default is 1.
#'
#' @param n order of matrix
#' @param lambda eigenvalue of Jordan block
#'
#' @export
jordbloc <- function(n, lambda = 1){
  J <- lambda * diag(n) + Diag(rep(1, n-1), 1)
  return(J)
}
