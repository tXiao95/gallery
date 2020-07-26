context("Lehmer matrix")

# lehmer: Lehmer matrix -----------------------------------------------------------

test_that("Lehmer matrix is correct", {
  A2 <- matrix(c(1, 1/2, 1/2, 1), nrow = 2)
  B2 <- lehmer(2)
  expect_equal(norm(A2 - B2), 0)

  A3 <- matrix(c(1, 1/2, 1/3, 1/2, 1, 2/3, 1/3, 2/3, 1), nrow = 3)
  B3 <- lehmer(3)
  expect_equal(norm(A3 - B3), 0)

  A4 <- matrix(c(1, 1/2, 1/3, 1/4, 1/2, 1, 2/3, 1/2, 1/3, 2/3, 1, 3/4, 1/4, 1/2, 3/4, 1), nrow = 4)
  B4 <- lehmer(4)
  expect_equal(norm(A4 - B4), 0)
})
