
# The idea
#
# The idea is based on moving point(s), calculating new distance matrix and
# calculating error (mean or max) which is related with difference between this
# new distance matrix and distance matrix which is input for this algorithm.
# Algorithm tries to minimize it.


# Take initial step
# Prepare distance matrix for initial position
#
# In one iteration:
# For every point
#   For every dimension
#     Move point and calculate new distance matrix. If error is smaller than
#     for previous distance matrix keep this new position and new related distance matrix.
#     If you fail, try to move in second direction.
#
# If there was no one optimization during whole iteration, divide step by 2 in next iteration.
# Remark: to keep points close (0,0,...,0) (so to not move to far during many iterations)
# in every iteration randomly choose point order, dim order and direction (- or +).
#

# n_dim - number of dimensions
algorithm_1 <- function(dm, error_thr, fnc, n_dim, max_iter) {
  if(!(fnc %in% c("max", "mean")))
    stop("Available options for fnc parameter are: max or mean")

  num_points <- nrow(dm)

  # For mean error, start from (0,0,..,0) point
  # For max error , start from randomly distributed points

  if (fnc == "mean") {
    current_position <- matrix(0,num_points, n_dim)
  } else {
    current_position <- prepare_initial_current_position(num_points,
                                                         n_dim,
                                                         mean(dm))
  }
  current_dm <- prepare_current_distance_matrix(current_position)

  dist_err <- calculate_distance_error(dm, current_dm, fnc)

  step <- mean(dm)
  i <- 0
  error <- Inf

  while(i < max_iter & error > error_thr) {
    optimization_in_current_round <- FALSE
    i <- i + 1
    # draw the order of points
    point_set <- sample(1:num_points, num_points)
    for(point_index in point_set) {
      # draw the order of dimensions
      dim_set <- sample(1:n_dim, n_dim)
      for(dim_index in dim_set) {
        result <- optimize_in_one_dimension(dm, current_dm,  fnc, current_position,
                                                        step, point_index,  dim_index)

        if(result[["optimization_made"]]) {
          optimization_in_current_round <- TRUE

          current_dm <- result[["current_dm"]]
          current_position <- result[["current_position"]]
          error <- result[["error"]]
          #break # only one successful dim?
        }
      }
    }

    step_to_print <- step
    # If we could make optimization with current step
    if(!optimization_in_current_round) {
      step <- step / 2
    }

    print(paste0(i,": error:", error, " step: ", step_to_print))

  }

  print(" current distance matrix")
  print(current_dm)

  print("current position")
  print(current_position)

  l <- list()
  l[["positions"]] <- current_position
  l[["error"]] <- error
  return(l)
}



# current_position  matrix with dimensions (num_points, num_dimensions)

# returns list result
# result["optimization_made"] - was optimization made? (TRUE/FALSE)
# if result["optimization_made"] == TRUE, two additional items are added
# result[["current_dm"]] - distance matrix
# result[["current_position"]] - current position (matrix)
# result[["error"]] - error
optimize_in_one_dimension <- function(dm, current_dm,  method, current_position,
                                      step, point_index,  dim_index) {
  step <- step*sample(c(-1,1),1)
  optimization_made <- FALSE
  error <- calculate_distance_error(dm, current_dm,  method)
  num_of_points <- nrow(dm)
  num_of_dimensions <- ncol(current_position)

  l <- go_and_check_error(dm, current_dm,  method, current_position,
                step, point_index,  dim_index)

  if(!l[["optimization_made"]]) {
    step <- -1*step
    l <- go_and_check_error(dm, current_dm,  method, current_position,
                            step, point_index,  dim_index)
  }

  result <- list()

  result[["optimization_made"]] <- l[["optimization_made"]]
  if(result[["optimization_made"]]) {
    result[["current_dm"]] <- l[["current_dm"]]
    result[["current_position"]] <- l[["current_position"]]
    result[["error"]] <- l[["error"]]
  }

  return(result)
}


# current_position  matrix with dimensions (num_points, num_dimensions)

# returns list l
# l["optimization_made"] - was optimization made? (TRUE/FALSE)
# if l["optimization_made"] == TRUE, two additional items are added
# l[["current_dm"]] - distance matrix
# l[["current_position"]] - current position (matrix)
# l[["error"]] - error

go_and_check_error <- function(dm, current_dm,  method, current_position,
                               step, point_index,  dim_index) {

  num_of_points <- nrow(dm)
  num_of_dimensions <- ncol(current_position)
  error <- calculate_distance_error(dm, current_dm,  method)
  optimization_made <- FALSE

  while(TRUE) {
    current_dm_tmp <- current_dm
    current_position_tmp <- current_position

    # go in one direction (by step)
    current_position_tmp[point_index, dim_index] <-
      current_position_tmp[point_index, dim_index] + step

    # update (temporary) distance matrix for point_point_index vs all points
    for(i in 1:num_of_points) {
      # calculate distance for pair (point_point_index, point_i)
      distance <- 0
      for(j in 1:num_of_dimensions) {
        distance <- distance +
          (current_position_tmp[point_index, j] - current_position_tmp[i, j])^2
      }
      distance <- sqrt(distance)

      if(i < point_index) {
        current_dm_tmp[point_index, i] <- distance
      } else {
        current_dm_tmp[i, point_index] <- distance
      }
    }
    # check new error
    tmp_error <- calculate_distance_error(dm, current_dm_tmp,  method)

    # is it smaller than previous?
    if (tmp_error < error) {
      current_dm <- current_dm_tmp
      current_position <- current_position_tmp
      error <- tmp_error

      optimization_made <- TRUE
    }
    else {
      break
    }
  }

  l <- list()
  l[["optimization_made"]] <- optimization_made
  if(optimization_made) {
    l[["current_dm"]] <- current_dm  ## Tu sprawdzic
    l[["current_position"]] <- current_position
    l[["error"]] <- error
  }

  return(l)

}




