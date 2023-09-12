subset_by_day <- function(x, days = NULL,
                          before = NULL, after = NULL,
                          except = NULL) {
  #' Subset a SpatRaster based on the layers' day
  #'
  #' @description Easily select only the layers of a SpatRaster depending on
  #'   only the layers' day. For example, only data on the 1st of a month (1),
  #'   or only data between the 10th and 15th of a month (10:15).
  #'
  #'   This function is distinct to `subset_by_date()` because it subsets
  #'   using *only* the day part of the date: there is no accounting for the
  #'   month or year. This approach can be useful to select the first day of
  #'   each month for example.
  #'
  #' @inheritParams subset_by
  #' @param days Which day/s to return? Use this argument for exact matches
  #'   (e.g. c(1, 8, 15)), otherwise leave this as NULL (the default) and use
  #'   one of the other arguments.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  xSubset <- subset_by(x, type = "day",
                       exact = minutes,
                       before = before, after = after,
                       except = except)
  return(xSubset)
}
