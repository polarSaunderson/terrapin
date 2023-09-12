subset_by_minute <- function(x, minutes = NULL,
                             before = NULL, after = NULL,
                             except = NULL) {
  #' Subset a SpatRaster based on the layers' minute
  #'
  #' @description Easily select only the layers of a SpatRaster depending on
  #'   only the layers' minute. For example, only data at quarter past the hour
  #'   (15) or only data in the last half of an hour (>30). Works only on the
  #'   minute (ignores hours!) and if time information is available in the 'x'
  #'   raster.
  #'
  #' @inheritParams subset_by
  #' @param hours Which minute/s to return? Use this argument for exact matches
  #'   (e.g. c(0:15, 30:40)), otherwise leave this as NULL (the default) and use
  #'   one of the other arguments.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  xSubset <- subset_by(x, type = "minute",
                       exact = minutes,
                       before = before, after = after,
                       except = except)
  return(xSubset)
}
