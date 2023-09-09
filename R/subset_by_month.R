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
  #' @param months vector: Which month/s to return? Input is fed directly into
  #'   [get_months()] to handle different formats, but either 12, "12", "Dec",
  #'   "December", "dec", or "december" should work.
  #' @param excludeIncomplete Be careful using this argument! It can
  #'   dramatically affect the output of this function. Please read the
  #'   Explanation in [exclude_incomplete_years()] first.
  #'
  #'   If TRUE, the data is run through `exclude_incomplete_years()`, and only
  #'   months in years with all requested months in are returned.
  #'
  #'   If numeric (between 1 and 12), the data is fed into
  #'   `exclude_incomplete_summers()` and the value is used as the
  #'   'australSplit' argument to return only months in austral summers that
  #'   contain all requested months.
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
  # Handle different monthly inputs; uses `retrieve_months` rather than just
  # `handle_months` so that full dates can be entered.
  months <- get_months(x = months, out = 1, throwError = TRUE)

  # Handle if x is a filename
  if ("SpatRaster" %notIn% methods::is(x)) {
    x <- terra::rast(x)
  }

  # Handling Dates -------------------------------------------------------------
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
