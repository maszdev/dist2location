context("Repair Distance Matrix")


#TEST DATA FOR NA FIX

m1 <- matrix(rep(NA,9),3,3)
exp_fix_m1 <- matrix(c(0,1,1,
                       1,0,1,
                       1,1,0),3,3)


m2 <- matrix(c(NA,5,NA,
               NA,NA,NA,
               NA,NA,NA),3,3)

exp_fix_m2 <- matrix(c(0,5,5,
                       5,0,5,
                       5,5,0),3,3)

m3 <- matrix(c(1,5,NA,
               2,NA,5,
               100,4,0),3,3)

exp_fix_m3 <- matrix(c(1,5,100,
                       2,0,5,
                       100,4,0),3,3)

m4 <- matrix(c(0,NA,3,
               NA,NA,3,
               3,3,0),3,3)

exp_fix_m4 <- matrix(c(0,3,3,
                       3,0,3,
                       3,3,0),3,3)

m5 <- matrix(c(0,2,3,
               2,0,3,
               3,3,0),3,3)

# DATA FOR SQARE MATIX FIX
#m6 <- matrix(1:8,2,4)
m6 <- matrix(c(1,3,5,7,
               2,4,6,8),2,4,byrow=TRUE)
exp_fix_m6 <- matrix(c(1,3,5,7,
                       2,4,6,8,
                       2,4,6,8,
                       2,4,6,8),4,4,byrow=TRUE)

m7 <- matrix(c(1,3,5,7,
               2,4,6,8),4,2)
exp_fix_m7 <- matrix(c(1,3,5,7,
                       2,4,6,8,
                       2,4,6,8,
                       2,4,6,8),4,4)

# DATA FOR FIX METRIC RULE 1

m8 <- matrix(rep(0,9),3,3)
exp_fix_m8 <- matrix(c(0,1,1,
                       1,0,1,
                       1,1,0),3,3)

m9 <- matrix(c(2,2,0,
               2,0,2,
               2,2,0),3,3)

exp_fix_m9 <- matrix(c(0,2,2,
                       2,0,2,
                       2,2,0),3,3)

m10 <- matrix(c(3,3,0,
               3,0,3,
               0,3,3),3,3)

exp_fix_m10 <- matrix(c(0,3,3,
                       3,0,3,
                       3,3,0),3,3)

# DATA FOR FIX METRIC RULE 2
m11 <- matrix(c(0,3,1,4,
               3,0,1,1,
               1,1,0,1,
               4,1,1,0),4,4,byrow=TRUE)

m12 <- matrix(c(0,3,1,4,
                2,0,1,1,
                1,1,0,1,
                1,1,1,0),4,4,byrow=TRUE)

# DATA FOR FIX METRIC RULE 3
m13 <- matrix(c(0,4,2,2,
                4,0,2,2,
                2,2,0,2,
                2,2,2,0),4,4,byrow=TRUE)

m14 <- matrix(c(0,5,2,2,
                5,0,2,2,
                2,2,0,2,
                2,2,2,0),4,4,byrow=TRUE)

# DATA FOR GENERAL TESTS
m15 <- matrix(c(0,-4,2,2,
                4,0,2,2,
                2,2,0,2,
                2,2,2,0),4,4,byrow=TRUE)

m16 <- matrix(c(0,5,2,2,
                -5,0,2,2,
                2,2,0,2,
                2,2,2,0),4,4,byrow=TRUE)

m17 <- matrix(c(1,3,NA,
               2,NA,2.3,
               2,2.3,0),3,3,byrow=TRUE)

exp_fix_m17 <- matrix(c(0,3,2,
                       3,0,2.3,
                       2,2.3,0),3,3,byrow=TRUE)
#TEST FOR NA FIX

test_that("Repair, all NA", {
  expect_equal(fix.na(m1), exp_fix_m1)
})

test_that("Repair, only one number provided", {
  expect_equal(fix.na(m2), exp_fix_m2)
})

test_that("Repair, all NA can be replaced by known numbers", {
  expect_equal(fix.na(m3), exp_fix_m3)
})

test_that("Repair,NA has to be calculated by aver from other existing values", {
  expect_equal(fix.na(m4), exp_fix_m4)
})

test_that("No NA, no changes", {
  expect_equal(fix.na(m5), m5)
})

#TEST FOR SQUARE MATRIX FIX

test_that("Make square matrix, already square", {
  expect_equal(make_square_matrix(m5), m5)
})

test_that("Make square matrix, more rows", {
  expect_equal(make_square_matrix(m7), exp_fix_m7)
})

# TEST FOR FIX METRIC RULE 1

test_that("Repair metric rule 1, rule 1 fulfilled", {
  expect_equal(repair_metric_rule1(m5), m5)
})

test_that("Repair metric rule 1, all zeros", {
  expect_equal(repair_metric_rule1(m8), exp_fix_m8)
})

test_that("Repair metric rule 1, zeros in wrong place, example 1", {
  expect_equal(repair_metric_rule1(m9), exp_fix_m9)
})

test_that("Repair metric rule 1, zeros in wrong place, example 2", {
  expect_equal(repair_metric_rule1(m10), exp_fix_m10)
})

# TEST FOR FIX METRIC RULE 2

test_that("Repair metric rule 2, rule 2 fulfilled", {
  expect_equal(repair_metric_rule2(m11), m11)
})

test_that("Repair metric rule 2, proper fix", {
  expect_equal(repair_metric_rule2(m12), m11)
})

# TEST FOR FIX METRIC RULE 3

test_that("Repair metric rule 3, rule 3 fulfilled", {
  expect_equal(repair_metric_rule3(m13), m13)
})

test_that("Repair metric rule 3, proper fix", {
  expect_equal(repair_metric_rule3(m14), m13)
})

# TEST FOR MAIN REPAIR FUNCTION

test_that("Repair dm, dm OK", {
  expect_equal(repair_distance_matrix(m13), m13)
})

test_that("Repair dm, negative value", {
  expect_equal(repair_distance_matrix(m15), m13)
})

test_that("Repair dm, negative value and 3rd rule broken", {
  expect_equal(repair_distance_matrix(m16), m13)
})

test_that("Repair dm, fix NA, 1,2 and 3 metric rule", {
  expect_equal(repair_distance_matrix(m17), exp_fix_m17)
})
