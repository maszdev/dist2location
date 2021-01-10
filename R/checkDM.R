#' Check if matrix fulfills distance matrix conditions
#'
#' The `is_distance_matrix_ok()` function is used to validate matrix.
#'
#' There are distance matrix conditions which should be fulfilled:
#'
#' 3 distance rules
#' \enumerate{
#'  \item d(x,y) = 0 <=> x = y
#'  \item d(x,y) = d(y,x)
#'  \item d(x,y) <= d(x,z) + d(y,z)
#' }
#' Additionally matrix should have the same number of rows and columns,
#' and negative or NA values should not be included.
#'
#' @param m Distance Matrix which is to be validate
#' @return If all conditions are fulfilled TRUE is returned, else FALSE.
#' @export
is_distance_matrix_ok <- function(m) {
  if(!is_square_matrix(m)) stop("This is not a square matrix")

  if(!(sum(is.na(m)) == 0)) return (FALSE)

  if(!(sum(m<0) == 0)) return(FALSE)

  if(!is_metric_rule1_fulfilled(m)) return (FALSE)

  if(!is_metric_rule2_fulfilled(m)) return (FALSE)

  if(!is_metric_rule3_fulfilled(m)) return (FALSE)

  return (TRUE)
}


#' Debug distance matrix
#'
#' The debug_distance_matrix() function is used for troubleshooting.
#'
#' If distance matrix is to be used by to find points, it has to be well
#' defined. This function checks if matrix is square, if doesn't include NA or
#' negative values. If those conditions are not fulfilled proper information is
#' printed.
#'
#' Additionally metric rules are checked:
#'
#' \enumerate{
#'  \item d(x,y) = 0 <=> x = y
#'  \item d(x,y) = d(y,x)
#'  \item d(x,y) <= d(x,z) + d(y,z)
#' }
#' If some rule is not fulfilled indexes of points/distance which cause
#' problems are printed
#'
#' @param m Distance Matrix which is to be validate
#' @export
debug_distance_matrix <- function(m) {
  if(!is_square_matrix(m)) stop("This is not a square matrix")

  basic_check_ok <- TRUE
  if(sum(is.na(m)) == 0) {
    cat("No NA in matrix: OK\n")
  }
  else {
    cat ("NA found in matrix: <-- ERROR\n")
    basic_check_ok <- FALSE
  }

  if(sum(m<0, na.rm = TRUE) == 0) {
    cat("No negative values in matrix: OK\n\n")
  }
  else {
    cat ("Negative values found in matrix\n\n")
    basic_check_ok <- FALSE
  }

  if (!basic_check_ok) {
    cat("Metric rules are not checked.",
            "Please correct problem(s) mentioned above first.\n")
    return ("")
  }

  cat("First metric rule:")
  debug_info <- debug_matrix_for_metric_rule1(m)
  if(debug_info == "") cat(" OK\n")
  else cat(" Not OK\n",debug_info)

  cat("Second metric rule:")
  debug_info <- debug_matrix_for_metric_rule2(m)
  if(debug_info == "") cat(" OK\n")
  else {
    cat(" Not OK\n",debug_info,  "\nChecking third rule skipped.",
        "Correct problems with second rule first\n")
    return("")
  }

  cat("Third metric rule:")
  debug_info <- debug_matrix_for_metric_rule3(m)
  if(debug_info == "") cat(" OK\n")
  else cat(" Not OK\n", debug_info)

  return("")

}


#' Function returns TRUE for square matrix, else FALSE is returned
#'
is_square_matrix <- function(m) {
  stopifnot(is.matrix(m))

  is_square <- FALSE
  if(ncol(m) == nrow(m)) {
    is_square <- TRUE
  }

  return (is_square)
}


#' Function checks if first metric rule is fulfilled
#'  d(x,y) = 0 <=> x = y
is_metric_rule1_fulfilled <- function(m) {
  if(!is_square_matrix(m)) stop("This is not a square matrix")

  debug_info1 = c()
  debug_info2 = c()
  n <- nrow(m)

  for (i in 1:n) {
    for (j in 1:n) {
      if(m[i,j] == 0) {
        if(i!=j) return(FALSE)
      }
      else {
        if(i==j) return(FALSE)
      }
    }
  }

  return(TRUE)
}

#' Function debug matrix content according to first metric rule
#'  d(x,y) = 0 <=> x = y
#'
#' Printout returned includes information about point which cause problems.
#' If first metric rule is fulfilled, empty string is returned.
#'
debug_matrix_for_metric_rule1 <- function(m) {
  if(!is_square_matrix(m)) stop("This is not a square matrix")

  debug_info1 = "" #info about pair of points (x,y) for which d(x,y) = 0
  debug_info2 = "" #info about points x for which d(x,x) != 0
  n <- nrow(m)
  result <- ""
  for (i in 1:n) {
    for (j in 1:n) {
      if(m[i,j] == 0) {
        if(i!=j) {
          debug_info1 <- paste0(debug_info1, " ", paste(i,j,sep = ","))
        }
      }
      else {
        if(i==j) {
          debug_info2 <- paste0(debug_info2, " ", toString(i))
        }
      }
    }
  }

  if(debug_info1 != "") {
    result <- "Position in matrix for pair of points for which d(x,y) = 0\n"
    result <- paste0(result,debug_info1, "\n")
  }
  if(debug_info2 != "") {
    result <- paste0(result,"Points for which d(x,x) != 0\n")
    result <- paste0(result,debug_info2 ,"\n")
  }

  return (result)
}


#' Function checks if second metric rule is fulfilled
#' d(x,y) = d(y,x)
is_metric_rule2_fulfilled <- function(m) {
  if(!is_square_matrix(m)) stop("This is not a square matrix")

  debug_info = c()
  n <- nrow(m)

  for (i in 1:n) {
    for (j in 1:n) {
      if(i <= j) next
      if (m[i,j] != m[j,i]) return(FALSE)
    }
  }

  return(TRUE)
}


#' Function debug matrix content according to second metric rule
#'  d(x,y) = d(y,x)
#'
#' Printout returned includes information about pair of point which
#' cause problems.
#' If second metric rule is fulfilled, empty string is returned.
#'
debug_matrix_for_metric_rule2 <- function(m) {
  if(!is_square_matrix(m)) stop("This is not a square matrix")

  debug_info = ""
  result <- ""
  n <- nrow(m)

  for (i in 1:n) {
    for (j in 1:n) {
      if(i <= j) next
      if (m[i,j] != m[j,i]) {
        debug_info <- paste0(debug_info,paste(i,j,sep = ","))
      }
    }
  }
  if(debug_info != "" ) {
    result <- "Position in matrix for pairs of points for which d(x,y) != d(y,x)"
    result <- paste0(result,"\n",debug_info,"\n")
  }

  return(result)
}


#' Function checks if third metric rule is fulfilled
#' d(x,z) <= d(x,y) + d(x,z)
#' Warning: algorithm works properly when rule2 is fulfilled
#' @importFrom utils combn
is_metric_rule3_fulfilled <- function(m) {
  if(!is_square_matrix(m)) stop("This is not a square matrix")
  if (nrow(m) < 3) return (TRUE)

  debug_info = c()

  is_triple_ok <- function(t) {
    if(m[t[1],t[3]] > m[t[2],t[3]] + m[t[1],t[2]]) return (FALSE)
    if(m[t[2],t[3]] > m[t[1],t[2]] + m[t[1],t[3]]) return (FALSE)
    if(m[t[1],t[2]] > m[t[2],t[3]] + m[t[1],t[3]]) return (FALSE)
    return(TRUE)
  }

  all_triple_indexes <- combn(ncol(m),3)
  wrong_triples <- apply(all_triple_indexes,2,is_triple_ok)
  for (i in 1:length(wrong_triples)) {
    if(!wrong_triples[i]) return(FALSE)
  }

  return(TRUE)
}

#' Function debug matrix content according to third metric rule
#' d(x,z) <= d(x,y) + d(x,z)
#'
#' Printout returned includes information about triples of point which
#' cause problems.
#' If third metric rule is fulfilled, empty string is returned.
#'
#' Warning: algorithm works properly when rule2 is fulfilled
#' @importFrom utils combn
debug_matrix_for_metric_rule3 <- function(m) {
  if(!is_square_matrix(m)) stop("This is not a square matrix")
  if (nrow(m) < 3) return ("")

  debug_info = ""
  result <- ""

  is_triple_ok <- function(t) {
    if(m[t[1],t[3]] > m[t[2],t[3]] + m[t[1],t[2]]) return (FALSE)
    if(m[t[2],t[3]] > m[t[1],t[2]] + m[t[1],t[3]]) return (FALSE)
    if(m[t[1],t[2]] > m[t[2],t[3]] + m[t[1],t[3]]) return (FALSE)
    return (TRUE)
  }

  all_triple_indexes <- combn(ncol(m),3)
  wrong_triples <- apply(all_triple_indexes,2,is_triple_ok)

  for (i in 1:length(wrong_triples)) {
    if(!wrong_triples[i]) {
      t <- all_triple_indexes[,i]
      debug_info <- paste0(debug_info,paste(t[1],t[2],t[3],sep = ","))
    }
  }

  if(debug_info != "") {
    result <- "Indexes for triples of points for which d(x,z) < d(x,y) + d(x,z)"
    result <- paste0(result,"\n",debug_info,"\n")
  }

  return(result)
}
