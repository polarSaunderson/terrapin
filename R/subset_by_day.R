subset_by_day <- function(x, days) {
  #' Subset a SpatRaster based on the layers' day
  #'
  #' @description Easily select only layers of a SpatRaster that are on a
  #'   certain day/s. For example, only want layers that are on the 1st of a
  #'   month. This function is distinct to `subset_by_date()` as it subsets
  #'   using *only* the day part of the date: there is no accounting for the
  #'   month or year. This approach can be useful to select the first day of
  #'   each month for example.
  #'
  #' @param x SpatRaster: The data to subset. Can be either a string, in which
  #'   case it is interpreted as a filePath and read in, or an existing
  #'   SpatRaster.
  #' @param days vector: Which day/s to return?
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Get dates of each layer
  xDates <- get_terra_dates(x, australSplit = NULL)

  # Identify relevant layers
  dayIndex <- which(xDates$day %in% days)

  # Subset the data
  xSubset <- terra::subset(x, dayIndex)

  return(xSubset)
}
