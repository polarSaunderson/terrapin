subset_by_year <- function(x, years) {
  #' Subset a SpatRaster based on the layers' year
  #'
  #' @description Easily select only layers of a SpatRaster that are in a
  #'   certain year/s. For example, only want layers that are in 1991.
  #'
  #' @param x SpatRaster: The data to subset. Can be either a string, in which
  #'   case it is interpreted as a filePath and read in; or an existing
  #'   SpatRaster.
  #' @param years vector: Which year/s to return?
  #'
  #' @examples
  #' \dontrun{
  #'   x <- terra::rast("fileName.nc")
  #'
  #'   # Only 1991
  #'   x91 <- terrapin::subset_by_year(x, 1991)
  #'
  #'   # From 1991 to 1994
  #'   x9194 <- terrapin::subset_by_year(x, 1991:1994)
  #' }
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Get dates of each layer
  xDates <- get_terra_dates(x, australSplit = NULL)

  # Identify relevant layers
  yearlyIndex <- which(xDates$year %in% years)

  # Subset the data
  xSubset <- terra::subset(x, yearlyIndex)

  return(xSubset)
}
