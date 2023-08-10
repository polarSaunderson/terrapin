subset_by_month <- function(x, months,
                            removeIncomplete = FALSE,
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
  #' @param removeIncomplete If the value is "years", the data is run through
  #'   `remove_incomplete_years()`, and only months in years with all requested
  #'   months in are returned. If the value is numeric (between 1 and 12), the
  #'   data is fed into the `remove_incomplete_austral_summers()` and the value
  #'   is used as the "australSplit" argument to return only months in austral
  #'   summers that contain all requested months. If any other value, all layers
  #'   matching the months argument are returned, regardless of the summer or
  #'   year. See the examples.
  #' @param dailyResolution BINARY: If using the "removeIncomplete" argument, it
  #'   is necessary to define whether the data is at a daily or monthly
  #'   resolution. See the `remove_incomplete_austral_summers()` and
  #'   `remove_incomplete_years()` functions for more information.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Handle if months is "dec", "Dec", "December" or "december" instead of 12
  if (isFALSE(is.numeric(months[1]))) {
    months <- which(tolower(month.abb) %in% substring(tolower(months), 1, 3))
  }

  # Get dates of each layer
  xDates <- get_terra_dates(x, austral = NULL)

  # Identify relevant layers
  monthlyIndex <- which(xDates$month %in% months)

  # Subset the data
  xSubset <- terra::subset(x, monthlyIndex)

  # Remove any summers or years without all of the necessary months
  if ("numeric" %in% is(removeIncomplete)) {
    xSubset <- remove_incomplete_austral_summers(xSubset,
                                                 daily = dailyResolution,
                                                 australSplit = removeIncomplete)
  } else if (removeIncomplete == "years") {
    xSubset <- remove_incomplete_years(xSubset, daily = dailyResolution)
  }

  return(xSubset)
}
