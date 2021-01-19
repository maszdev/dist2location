context("Create Distance Matrix")

# DATA FOR VALIDATION TESTS

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

# DATA FOR FORMAT1 TESTS

df_format1 <- data.frame (first_column  = c(1,3,1),
                          second_column = c(2, 2,3),
                          third_column = c(2.1, 7.3,4))

df_format1_index_double <- data.frame (first_column  = c(1.1,3,1),
                          second_column = c(2, 2.9,3),
                          third_column = c(2.1, 7.3,4))

#one point pairs repeated twice (with different distance)
#one distance between the same point not equal to 0
df_format1_with_additions <- data.frame (first_column  = c(3,1,3,1,2),
                          second_column = c(1,2, 2,3,2),
                          third_column = c(10,2.1, 7.3,4,1))

df_format1_uncompleted <- data.frame (first_column  = c(1,3,1),
                                      second_column = c(2,2,4),
                                      third_column = c(2.1,7.3,4))

df_format1_uncompleted2 <- data.frame(first_column  = c(1),
                                    second_column = c(4),
                                    third_column = c(2))


expected_m_1 <- matrix(c(0,2.1,4,
                       2.1,0,7.3,
                       4,7.3,0),3,3)


expected_m_uncomplited <- matrix(c(0,2.1,NA,4,
                                  2.1,0,7.3,NA,
                                  NA,7.3,0,NA,
                                  4,NA,NA,0),4,4)

expected_m_uncomplited2 <- matrix(c(0,NA,NA,2,
                                    NA,0,NA,NA,
                                    NA,NA,0,NA,
                                    2,NA,NA,0),4,4)


# DATA FOR FORMAT2 TESTS
df_format2 <- data.frame (first_column  = c("A","C","A"),
                          second_column = c("B", "B","C"),
                          third_column = c(2.1, 7.3,4))

df_format2_uncompleted <- data.frame (first_column  = c("C","A","B","D"),
                                      second_column = c("D","B","C","A"),
                                      third_column = c(2,3,4,5))

df_format2_with_additions <- data.frame (first_column  = c("C","A","C","A","A"),
                                      second_column = c("B", "B", "B","C","A"),
                                      third_column = c(11,2.1, 7.3,4,12))

expected_m_2 <- matrix(c(0,2.1,4,
                         2.1,0,7.3,
                         4,7.3,0),3,3)

colnames(expected_m_2) <- c("A","B","C")
rownames(expected_m_2) <- c("A","B","C")

expected_m2_uncompleted <- matrix(c(0,3,NA,5,
                                    3,0,4,NA,
                                    NA,4,0,2,
                                    5,NA,2,0),4,4)

colnames(expected_m2_uncompleted) <- c("A","B","C","D")
rownames(expected_m2_uncompleted) <- c("A","B","C","D")



# VALIDATION
#


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

test_that("Validate, format 1", {
  expect_equal(validate_df(df_format1), 1)
})

test_that("Validate, format 1, double instead of integer", {
  expect_equal(validate_df(df_format1_index_double), 1)
})


## TEST FOR FORMAT 1

test_that("Matrix, format 1", {
  expect_equal(create_distance_matrix(df_format1), expected_m_1)
})

test_that("Matrix, format 1, indexes has double type", {
  expect_equal(create_distance_matrix(df_format1_index_double), expected_m_1)
})

test_that("Matrix, format 1, not needed info", {
  expect_equal(create_distance_matrix(df_format1_with_additions), expected_m_1)
})

test_that("Matrix, format 1, uncompleted info", {
  expect_equal(create_distance_matrix(df_format1_uncompleted),
               expected_m_uncomplited)
})

test_that("Matrix, format 1, uncompleted info 2", {
  expect_equal(create_distance_matrix(df_format1_uncompleted2),
               expected_m_uncomplited2)
})

## TEST FOR FORMAT 2

test_that("Matrix, format 2", {
  expect_equal(create_distance_matrix(df_format2), expected_m_2)
})

test_that("Matrix uncompleted, format 2", {
  expect_equal(create_distance_matrix(df_format2_uncompleted),
               expected_m2_uncompleted)
})

test_that("Matrix, format 2, df with additions", {
  expect_equal(create_distance_matrix(df_format2_with_additions), expected_m_2)
})
