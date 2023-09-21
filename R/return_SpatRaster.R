return_SpatRaster <- function(x) {
  #' Handle whether x is a filename or SpatRaster
  #'
  #' @param x The input to check. If a string, it is fed into [terra::rast()] on
  #'   the assumption it is a file path. If a SpatRaster, returned as is.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  if ("SpatRaster" %notIn% methods::is(x)) {
    x <- terra::rast(x)
  }
  return(x)
}
