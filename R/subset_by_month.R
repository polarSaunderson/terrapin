subset_by_month <- function(x, months = NULL,
                            excludeIncomplete = FALSE,
                            dailyResolution = FALSE,
                            before = NULL, after = NULL,
                            except = NULL) {
  #' Subset a SpatRaster based on the layers' month
  #'
  #' @description Easily select only the layers of a SpatRaster depending on
  #'   only the layers' month. For example, only data in June, or only data
  #'   after October.
  #'
  #' @inheritParams subset_by
  #' @param months Which month/s to return? Use this argument for exact matches
  #'   (e.g. c(1:4, 8). Cannot be used in conjunction with the 'before', 'after',
  #'   or 'except' arguments.
  #'
  #'   **Note:** This argument is fed through [terrapin::get_months()], so this
  #'   argument can be input in different formats. For example, December can be
  #'   input as any of: 12, "12", "Dec", "dec", "December", or "december".
  #'
  #' @param excludeIncomplete Be careful using this argument! It can
  #'   dramatically affect the output of this function. Please read the
  #'   Explanation in [exclude_incomplete_years()] first.
  #'
  #'   If TRUE, the data is run through `exclude_incomplete_years()`, and
  #'   only data from the years containing all requested months are returned.
  #'
  #'   If numeric (between 1 and 12), the data is run through
  #'   [exclude_incomplete_summers], and the value is used as the 'australSplit'
  #'   argument. This returns only data from the austral summers that contain
  #'   all requested months.
  #'
  #'   If any other value (including the default FALSE), the
  #'   `exclude_incomplete_x` functions are skipped, and all layers matching the
  #'   'months' argument are returned, regardless of the summer or year in which
  #'   they occur. See the examples.
  #' @param dailyResolution BINARY: If using the 'excludeIncomplete' argument,
  #'   it is necessary to define whether the data is at a daily or monthly
  #'   resolution. See [exclude_incomplete_summers()] and
  #'   [exclude_incomplete_years()] for more information.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  xSubset <- subset_by(x, type = "month",
                       exact = months,
                       before = before, after = after,
                       except = except)

  # Remove any summers or years without all of the necessary months
  if (excludeIncomplete %in% 1:12) {
    xSubset <- exclude_incomplete_summers(xSubset,
                                          daily = dailyResolution,
                                          australSplit = excludeIncomplete)
  } else if (isTRUE(excludeIncomplete)) {#} == "years") {
    xSubset <- exclude_incomplete_years(xSubset, daily = dailyResolution)
  }

  return(xSubset)
}




