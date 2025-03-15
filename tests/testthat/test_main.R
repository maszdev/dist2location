
input_matrix <- function() {
  input <- matrix(c(0,1,1,1,
                    1,0,1,1,
                    1,1,0,1,
                    1,1,1,0), nrow = 4, ncol = 4, byrow = TRUE)
}


test_that("Algorithm 1, error mean", {
  dm <- input_matrix()
  result <- points_positions(dm)

  expect_true(result$error < 0.001)

})


test_that("Algorithm 1, error max", {
  dm <- input_matrix()
  result <- points_positions(dm, fnc = "max")

  expect_true(result$error < 0.001)
})
