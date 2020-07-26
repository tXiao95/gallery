context("Cauchy matrix")

# cauchy_matrix: Cauchy matrix --------------------------------------------

test_that("Cauchy matrix is cauchy", {
  set.seed(1)
  x <- rnorm(10)
  y <- rnorm(10)

  A <- cauchy_matrix(x, y)

  # Check identity of each matrix cell is correct for random x and y
  for(i in 1:nrow(A)){
    for(j in 1:ncol(A)){
      expect_equal(A[i,j], 1 / (x[i] + y[j]))
    }
  }

  # Two equal length vectors
  x <- 1:3
  y <- 2:4
  A <- cauchy_matrix(x, y)
  B <- matrix(c(1/3, 1/4,1/5,1/4,1/5,1/6,1/5,1/6,1/7),nrow=3)
  expect_equal(norm(A-B),0)

  # Scalar x
  x <- 3
  A <- cauchy_matrix(x)
  B <- matrix(c(1/2, 1/3,1/4,1/3,1/4,1/5,1/4,1/5,1/6),nrow=3)
  expect_equal(norm(A-B),0)

  # unequal lengths
  x <- 2:3
  y <- 1:100
  expect_error(cauchy_matrix(x, y))
})
