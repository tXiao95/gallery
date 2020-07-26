context("Tridiagonal matrices")

# tridiag: Sparse tridiagonal matrix ------------------------------------------------------

test_that("Tridiagonal matrix eigenvalues", {
  # Are tridiagonal matrix eigenvalues satisfied by identity
  n <- 50
  x <- 10
  y <- 2
  z <- 3

  eigs_analytic <- sort(y + 2*sqrt(x*z)*cos((1:n)*pi/(n+1)))
  A <- tridiag(n, x, y, z)
  eigs <- sort(eigen(A)$values)

  expect_equal(eigs, eigs_analytic, tolerance = 0.00001)
})

# test_that("Tridiagonal matrix is tridiagonal", {
#   n <- 1000
#   x <- 10
#   y <- 2
#   z <- 3
#   A <- tridiag(n, x, y, z)
#
#   for(i in 1:(n-1)){
#     A[i,i] <- A[i+1,i] <- A[i,i+1] <- 0
#   }
#   A[n,n] <- 0
#   expect_equal(norm(A), 0, tolerance = 0.00001)
# })
