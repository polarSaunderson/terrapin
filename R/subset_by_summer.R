subset_by_summer <- function(x, summers = NULL,
                             before = NULL, after = NULL,
                             except = NULL,
                             australSplit = 3) {
  #' Subset a SpatRaster based on the layers' austral summer
  #'
  #' @description Easily select only the layers of a SpatRaster depending on
  #'   only the layers' summer. For example, only data in the summer of 1991/92
  #'   (indicated by 1992), or only data after summer 2000.
  #'
  #'   See the australSplit argument for defining which months are considered in
  #'   which austral summer.
  #'
  #' @inheritParams subset_by
  #' @param summers Which summer/s to return? Use this argument for exact
  #'   matches (e.g. c(1980:1990, 1995:1997), otherwise leave this as NULL (the
  #'   default) and use one of the other arguments.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  xSubset <- subset_by(x, type = "summer",
                       exact = summers,
                       before = before, after = after,
                       except = except,
                       australSplit = australSplit)
  return(xSubset)
}
