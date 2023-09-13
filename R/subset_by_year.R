subset_by_year <- function(x, years = NULL,
                           before = NULL, after = NULL,
                           except = NULL) {
  #' Subset a SpatRaster based on the layers' year
  #'
  #' @description Easily select only the layers of a SpatRaster depending on
  #'   only the layers' year. For example, only data in 1991, or only data after
  #'   1999.
  #'
  #' @inheritParams subset_by
  #' @param years Which year/s to return? Use this argument for exact matches
  #'   (e.g. c(1983:1986, 1991:1992). Cannot be used in conjunction with
  #'   the 'before', 'after' or 'except' arguments.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  xSubset <- subset_by(x, type = "year", exact = years,
                       before = before, after = after,
                       except = except)
  return(xSubset)
}
