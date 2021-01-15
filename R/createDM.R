#' Create distance matrix
#'
#' This function can create distance matrix from data frame with proper
#' structure.
#'
#' Distances between points should be defined in form of distance matrix (see
#' vignette for package for details). create_distance_matrix() function uses as
#' an input  data frame for which every row includes case: ("pointX","pointY",
#' distance_between_them). As an output proper distance matrix is generated.
#'
#' Format of input data frame:
#'
#' Format 1
#' \itemize{
#' \item first column - numeric
#' \item second column - numeric
#' \item third column - numeric
#' }
#' For such case we assume that n points are enumerated by integers 1 .. n and
#' first two columns include those indexes.
#'
#' Format 2
#' \itemize{
#' \item first column - character
#' \item second column - character
#' \item third column - numeric
#' }
#' For such case we are using points names instead of indexes. All used names
#' are sorted and than indexes (used in output distance matrix) will be
#' assigned to following points.
#'
#' When input data frame is not complete (not all possible distances are
#' described) missing cases will be marked in distance matrix as NA.
#'
#' create_distance_matrix secures first and second metrics rule (see vignette
#' for package) so even if this info is not included in input df output dm
#' will include 0 in diagonal.
#' When there are many (different) distances for the same points in df, output
#' dm will include only last (for both (i,j) and (j,i) cases - second metrics
#' rule fulfilled)
#' @export
create_distance_matrix <- function(df) {
  df_format <- validate_df(df)

}
#' Function validates data frame with distance cases (point1,point2,distance)
#' \itemize{
#' \item If values included in all three columns are numeric 1 is returned
#' \item If values included in first and second column are character and third
#' includes numeric values 2 is returned
#' }
#' Otherwise, this is not a valid data frame, error is generated
validate_df <- function(df) {
  if(!is.data.frame(df)) stop("Specified object is not a data frame")
  if(nrow(df) == 0) stop("Specified data frame has 0 rows")
  if(ncol(df) != 3) stop("Data frame should have 3 columns")
  if(!is.numeric(df[,3])) stop("Third column should include numeric values")

  format <- 0
  if(is.numeric(df[1,]) & is.numeric(df[2,])) format <- 1
  if(is.character(df[1,]) & is.character(df[2,])) format <- 2
  if (format == 0) stop(paste("First two columns should include numeric values",
                       "or first two columns should include character values"))

  return(format)
}

