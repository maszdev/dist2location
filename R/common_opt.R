# Calculate errors (error matrix) for given distance matrix and
# current distance matrix
# returns matrix N*N
calculate_error_matrix <- function(dm, current_dm) {
  N <- nrow(dm)
  dist_err <- matrix(NA,N,N)

  for(i in 1:N) {
    for(j in seq_len(i-1)) {
      dist_err[i,j] <- abs(dm[i,j] - current_dm[i,j])/ dm[i,j]
    }
  }
  return(dist_err)
}

## Error based on error matrix
calculate_distance_error <- function(dm, current_dm, method) {
  error_matrix <- calculate_error_matrix(dm, current_dm)

  error_vector <- error_matrix[lower.tri(error_matrix)]

  if(method == "mean") {
    error <- mean(error_vector)
  } else { # max
    error <- max(error_vector)
  }

  return(error)
}

# cp - Matrix with current position
# num rows - number of points
# num columns - dimension

# only low triangle will be filled in in output distance matrix
# because it will be used when error is calculated
# cp = current position
prepare_current_distance_matrix <- function(cp) {
  N <- nrow(cp) # number of points
  num_dim <- ncol(cp) # number of dimensions
  current_dm <- matrix(0, N, N)


  for(i in 1:N) {
    for(j in seq_len(i-1)) {
      distance <- 0
      for(k in 1:num_dim) {
        distance <- distance + (cp[i,k] - cp[j,k])^2
      }
      distance <- sqrt(distance)
      current_dm[i,j] <- distance
    }
  }

  return(current_dm)

}

prepare_initial_current_position <- function(n_points, n_dim, max_val) {
  matrix(runif(n_points*n_dim, min=-max_val, max=max_val),
         nrow=n_points, ncol=n_dim)
}

