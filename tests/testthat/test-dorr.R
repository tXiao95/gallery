context("Dorr matrix")

# dorr: Dorr matrix -------------------------------------------------------

test_that("Dorr matrix is correct", {
  # theta 0.01
  Atrue <- matrix(c(1.32, -1.16, 0,
                    -.16, .32, -.16,
                    0, -1.16, 1.32), nrow=3, byrow=T)
  Atest <- as.matrix(dorr(3))
  expect_equal(norm(Atest - Atrue), 0)

  # negative theta
  Btrue <- matrix(c(-31, 15, 0,
                    16, -32, 16,
                    0, 15, -31), nrow=3, byrow=T)
  Btest <- as.matrix(dorr(3, -1))
  expect_equal(norm(Btest - Btrue), 0)
})
