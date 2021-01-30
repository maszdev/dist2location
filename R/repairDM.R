#' Repair distance matrix
#'
#' This function can repair distance matrix to satisfy all conditions needed
#' for  proper input for algorithms which are looking for points positions.
#'
#' \itemize{
#' \item If matrix is not square, it will be extended to fulfill this condition
#' \item All NA will be replaced by real values
#' \item All negative values will be changed by abs function
#' \item First metric rule will be forced (d(x,y) = 0 <=> x = y)
#' \item Second metric rule will be forced (d(x,y) = d(y,x))
#' \item Third metric rule will be forced (d(x,z) <= d(x,y) + d(y,z))
#' }
#' All changes are performed in intelligent way to change original matrix as
#' less as possible
#'
#' @param m original distance matrix
#' @param max.iter = 5, repairing third metric rule can be a little bit tricky
#'  when there are many triples combination which don't fulfill it. In such
#'  situation many iterations are needed. If number of needed iteration exceeds
#'  max.iter, simple matrix will be returned when all possible distances between
#'  points will be equal to the mean distance in original matrix.
#' @return repaired distance matrix (or original if repairing is not needed)
#' @export
repair_distance_matrix <- function(m,max.iter = 5) {

  m <- make_square_matrix(m)
  m <- fix.na(m)
  m <- abs(m)

  m <- repair_metric_rule1(m)
  m <- repair_metric_rule2(m)
  m <- repair_metric_rule3(m,max.iter)

  if(!is_distance_matrix_ok(m)) {
    cat("For unknown reasons matrix was not repaired\n")
    cat("use is_distance_matrix_ok(...) to find out what is still wrong")
  }
  return(m)
}

#' Function adds columns or rows to make matrix square
make_square_matrix <- function(m) {
  if(is_square_matrix(m)) return (m)

  diff_length <- ncol(m) - nrow(m)
  more_columns <- diff_length > 0

  if(more_columns) {
    for(i in (nrow(m)+1):ncol(m)) {
      m <- rbind(m,m[nrow(m),])
    }
  }
  else {
    for (i in (ncol(m)+1):nrow(m)) {
      m <- cbind(m,m[,ncol(m)])
    }
  }

  return(m)
}


#' Function removes all NA from distance matrix
#'
#' NA on diagonal replaced by 0
#' NA for (i,j) replaced by (j,i) if exist
#' otherwise replaced by meand from existing number (or by 1 if they don't exist)
#' @param m Distance Matrix
#' @return distance matrix without NA
fix.na <- function(m) {
  stopifnot(is.matrix(m))
  if(!is_square_matrix(m)) stop("This is not a square matrix")

  if(sum(is.na(m)) == 0) return (m)
  n <- nrow(m)
  if(sum(is.na(m)) == n*n) {
    m1 <- matrix(rep(1,n*n),n,n)
    for(i in 1:n) m1[i,i] <- 0
    return (m1)
  }

  sumRealNum <- 0
  countRealNum <- 0

  for (i in 1:n) {
    for (j in 1:n) {
      if(is.na(m[i,j])) {
        if(i == j) m[i,j] <- 0
        else if (!is.na(m[j,i])) m[i,j] <- m[j,i]
      }
    }
  }
  #check if still some NA
  numNA <- sum(is.na(m))
  if(numNA == 0) return (m)

  sumRealNum <- sum(m,na.rm = TRUE)
  countRealNum <- n*n - numNA - n # -n because we should ignore 0 on diagonal
  m[is.na(m)] <- sumRealNum / countRealNum  # average from real/included dist.

  return(m)
}

#' Function repairs dimension matrix to fulfill
#' first metric rule d(x,y) = 0 <=> x = y
#' Remark: there is an assumption that NA are not included (fix.na() used first)

repair_metric_rule1 <- function(m) {
  if(!is_square_matrix(m)) stop("This is not a square matrix")
  if (is_metric_rule1_fulfilled(m)) return (m)

  n <- nrow(m)
  # all zeros case
  if (sum(abs(m)) == 0) {
    m <- matrix(rep(1,n*n),n,n)
    for (i in 1:n) m[i,i] <- 0
    return (m)
  }

  # Fill in diagonal with zeros
  for (i in 1:n) m[i,i] <- 0

  #be ready fo zeros outside diagonal
  # new mean value for all non zero values
  new_val <- sum(m) / (n*n - sum(m==0))


  for (i in 1:n) {
    for (j in 1:n) if(m[i,j] == 0 & i != j)  m[i,j] <- new_val
  }

  return(m)
}

#' Function repairs dimension matrix to fulfill
#' second metric rule d(x,y) = d(y,x)
#'
#' Upper triangular part overwrites lower triangular part of matrix
repair_metric_rule2 <- function(m) {
  if(!is_square_matrix(m)) stop("This is not a square matrix")

  n <- nrow(m)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i < j) m[j,i] <- m[i,j]
    }
  }
  return(m)
}

#' Function repairs dimension matrix to fulfill
#' third metric rule d(x,z) <= d(x,y) + d(y,z)
#' Warning: algorithm works properly when rule2 is fulfilled
#'
#'  @param m Distance Matrix
#'  @param max.iter = 5, repairing third metric rule can be a little bit tricky
#'  when there are many triples combination which don't fulfill it. In such
#'  situation many iteration is needed. If number of needed iteration exceeds
#'  max.iter, simple matrix will be returned when all possible distances between
#'  points will be equal to the mean distance in original matrix.
#'
#' @importFrom utils combn
repair_metric_rule3 <- function(m, max.iter = 5) {
  if(!is_square_matrix(m)) stop("This is not a square matrix")


  n <- nrow(m)
  new_val <- sum(m)/(n*n -n) # mean value which don't include diagonal zeros

  repair_triple <- function(t) {
    if(m[t[1],t[3]] > m[t[2],t[3]] + m[t[1],t[2]]) {
      m[t[1],t[3]] <<- m[t[2],t[3]] + m[t[1],t[2]]
      m[t[3],t[1]] <<- m[t[2],t[3]] + m[t[1],t[2]]
    }
    if(m[t[2],t[3]] > m[t[1],t[2]] + m[t[1],t[3]]) {
      m[t[2],t[3]] <<- m[t[1],t[2]] + m[t[1],t[3]]
      m[t[3],t[2]] <<- m[t[1],t[2]] + m[t[1],t[3]]
    }
    if(m[t[1],t[2]] > m[t[2],t[3]] + m[t[1],t[3]]) {
      m[t[1],t[2]] <<- m[t[2],t[3]] + m[t[1],t[3]]
      m[t[2],t[1]] <<- m[t[2],t[3]] + m[t[1],t[3]]
    }
  }

  all_triple_indexes <- combn(ncol(m),3)
  i = 0
  while(!is_metric_rule3_fulfilled(m)) {
    i <- i + 1
    #print(i)
    apply(all_triple_indexes,2,repair_triple)
    if(i == max.iter) break
  }

  #If subtle algorithm (which changes data as less as possible)
  # doesn't work, let's use more powerful method ;-)
  if(!is_metric_rule3_fulfilled(m)) {
    for (i in 1:n) {
      for (j in 1:n) {
        if (i!=j) m[i,j] <- new_val
      }
    }
  }

  return(m)
}
