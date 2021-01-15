context("Create Distance Matrix")

m <- m_ok <- matrix(c(0,1,1,
                      1,0,1,
                      1,1,0),3,3)

df_wrong1 <- data.frame (first_column  = c("A", "B","C"),
                  second_column = c(2, 3,1),
                  third_column = c(2.1, 7.3,4))

df_wrong2 <- data.frame (first_column  = c(1,2,3),
                         second_column = c("A","B","C"),
                         third_column = c("C","A","B"))

df_wrong3 <- data.frame (first_column  = c(1,2,3),
                         second_column = c(2, 3,1),
                         third_column = c(2.1, 7.3,4),
                         fourth_column = c(3.1,4.1,7))

df_wrong4 <- data.frame (first_column  = c(1,2,3),
                         second_column = c(2, 3,1))

test_that("Not a data frame", {
  expect_error(create_distance_matrix(m),
               "Specified object is not a data frame")
})

test_that("Wrong format, different point name conventions", {
  expect_error(create_distance_matrix(df_wrong1),
               paste("First two columns should include numeric values",
                     "or first two columns should include character values"))
})

test_that("Wrong format, third column is not numeric", {
  expect_error(create_distance_matrix(df_wrong2),
               "Third column should include numeric values")
})

test_that("Wrong format, to many columns", {
  expect_error(create_distance_matrix(df_wrong3),
               "Data frame should have 3 columns")
})

test_that("Wrong format, not enough columns", {
  expect_error(create_distance_matrix(df_wrong4),
               "Data frame should have 3 columns")
})
