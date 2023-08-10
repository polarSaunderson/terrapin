subset_by_month <- function(x, months,
                            excludeIncomplete = FALSE,
                            dailyResolution = FALSE) {
  #' Subset a SpatRaster based on the layers' month
  #'
  #' @description Easily select only layers of a SpatRaster that are in a
  #'   certain month/s (e.g. only layers that are in December).
  #'
  #' @param x SpatRaster: The data to subset. Can be either a string, in which
  #'   case it is interpreted as a filePath and read in, or an existing
  #'   SpatRaster.
  #' @param months vector: Which month/s to return? Input as either the month
  #'   number (12), the full month name ("December" or "december"), or the
  #'   abbreviated month name ("Dec" or "dec"). Multiple months can be input at
  #'   once (e.g. c(12, 1, 2)), but do not try to mix strings and numbers in the
  #'   vector.
  #' @param excludeIncomplete Be careful using this argument, it can
  #'   dramatically affect the output of this function. If the value is TRUE,
  #'   the data is run through `exclude_incomplete_years()`, and only months in
  #'   years with all requested months in are returned. If the value is numeric
  #'   (between 1 and 12), the data is fed into the
  #'   `exclude_incomplete_summers()` and the value is used as the
  #'   'australSplit' argument to return only months in austral summers that
  #'   contain all requested months. If any other value (including the default
  #'   FALSE), these functions are skipped, and all layers matching the months
  #'   argument are returned, regardless of the summers or years. See the
  #'   examples.
  #' @param dailyResolution BINARY: If using the 'excludeIncomplete' argument,
  #'   it is necessary to define whether the data is at a daily or monthly
  #'   resolution. See the `exclude_incomplete_summers()` and
  #'   `exclude_incomplete_years()` functions for more information.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Handle if months is "dec", "Dec", "December" or "december" instead of 12
  if (isFALSE(is.numeric(months[1]))) {
    months <- which(tolower(month.abb) %in% substring(tolower(months), 1, 3))
  }

  # Get dates of each layer
  xDates <- get_terra_dates(x, australSplit = excludeIncomplete)

  # Identify relevant layers
  monthlyIndex <- which(xDates$month %in% months)

  # Subset the data
  xSubset <- terra::subset(x, monthlyIndex)

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
