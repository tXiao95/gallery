context("Anti-Hadamard matrices")

# dramadah: Anti-hadamard matrix ------------------------------------------

test_that("Dramadah matrix is correct", {
  A1 <- dramadah(4, 1)
  A2 <- dramadah(4, 2)
  A3 <- dramadah(4, 3)

  refA1 <- matrix(c(1,0,0,1,1,1,0,0,0,1,1,0,1,0,1,1), nrow=4)
  refA2 <- matrix(c(1,0,0,0,1,1,0,0,0,1,1,0,1,0,1,1), nrow=4)
  refA3 <- matrix(c(1,0,1,0,1,1,0,1,0,1,1,0,0,0,1,1), nrow=4)
  expect_equal(norm(A1 - refA1), 0)
  expect_equal(norm(A2 - refA2), 0)
  expect_equal(norm(A3 - refA3), 0)
})
