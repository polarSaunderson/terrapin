subset_by_hour <- function(x, hours = NULL,
                           before = NULL, after = NULL,
                           except = NULL) {
  #' Subset a SpatRaster based on the layers' hour
  #'
  #' @description Easily select only the layers of a SpatRaster depending on
  #'   only the layers' hour. For example, only data at 12:00, or only data
  #'   after 17:00. Works only on the hour (ignores minutes!) and if time
  #'   information is available in the 'x' raster.
  #'
  #' @inheritParams subset_by
  #' @param hours Which hour/s to return? Use this argument for exact matches
  #'   (e.g. c(12:17, 21-22)), otherwise leave this as NULL (the default) and
  #'   use one of the other arguments. Must be in the 24-hour format.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  xSubset <- subset_by(x, type = "hour",
                       exact = hours,
                       before = before, after = after,
                       except = except)
  return(xSubset)
}
