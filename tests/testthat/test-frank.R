context("Frank matrices")

# Frank matrix ---------------------------------------------------------

test_that("Frank matrix", {
  F <- frank(4)
  A <- matrix(c(4,3,0,0,3,3,2,0,2,2,2,1,1,1,1,1),nrow=4)

  expect_equal(norm(F - A), 0)

  F <- frank(4, 1)

  expect_equal(norm(F - t(A[4:1, 4:1])), 0)
})
