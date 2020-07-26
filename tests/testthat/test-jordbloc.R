context("Jordan block")

# jordbloc ----------------------------------------------------------------

test_that("Jordan block is correct", {
  A1 <- jordbloc(4)
  A2 <- jordbloc(4, 2)

  refA1 <- matrix(c(1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1), nrow=4)
  refA2 <- refA1
  diag(refA2) <- 2

  expect_equal(norm(A1 - refA1), 0)
  expect_equal(norm(A2 - refA2), 0)
})
