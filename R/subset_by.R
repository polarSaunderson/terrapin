subset_by <- function(x, type, exact = NULL,
                      before = NULL, after = NULL,
                      except = NULL,
                      australSplit = 3) {
  #' Subset a SpatRaster based on the layers' dates
  #'
  #' @description Easily select layers of a SpatRaster depending on their date
  #'   (and time if present). This is designed as an internal function that some
  #'   of the other, simpler `subset_by_x` functions are based on.
  #'
  #' @param x SpatRaster: The data to subset. It can be either a string, in
  #'   which case it is interpreted as a filepath and read in using
  #'   [terra::rast()]; or an existing SpatRaster.
  #' @param type Which time/date type should the data be subset based on?
  #'   Options are (example using a date of 2019-11-29 and time of 12:30):
  #'
  #'   - year      2019
  #'   - month     11
  #'   - day       29
  #'   - summer    2020
  #'   - hour      12
  #'   - minute    30
  #'
  #' @param exact vector: Values that the 'type' value must match exactly. Only
  #'   one of the criteria arguments can be used at once.
  #' @param before numeric: The maximum value that the 'type' value can have.
  #'   Only one of the criteria arguments can be used at once.
  #' @param after numeric: The minimum value that the 'type' value can have.
  #'   Only one of the criteria arguments can be used at once.
  #' @param except vector: Values that the 'type' value cannot match.
  #' @param australSplit numeric: If not FALSE, create an additional column for
  #'   the austral summer / year. For example, December 1991 and January 1992
  #'   often need to be considered as part of summer 1991/1992 in the southern
  #'   hemisphere rather than in their respective years. The default value is 3,
  #'   which means that all months *AFTER* March (i.e. month 3) are considered
  #'   as part of the following austral summer / year (i.e. April 1991 -- March
  #'   1992 are all in austral summer / year 1992). Swap this value accordingly:
  #'   e.g. setting it as 4 means May 1991 -- April 1992 are all austral summer
  #'   / year 1992.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Guard: only one of the options can be used at once
  check_if_null(exact, before, after, except,
                stopIfNoNull = TRUE, noNullMessage = "No dates selected!")

  # Handle if x is a filename
  x <- terra::rast(x, keeptime = TRUE, keepunits = TRUE, props = TRUE)

  # Get dates of each layer
  xDates <- get_date_info(x, australSplit = australSplit)

  # Identify layers
  if (!is.null(exact)) {
    xIndex <- which(xDates[[type]] %in% exact)
  } else if (!is.null(before)) {
    xIndex <- which(xDates[[type]] < before)
  } else if (!is.null(after)) {
    xIndex <- which(xDates[[type]] > after)
  } else if (!is.null(between[1])) {
    xIndex <- which(xDates[[type]] >= between[1] & xDates[[type]] <= between[2])
  } else if (!is.null(except)) {
    xIndex <- which(xDates[[type]] %notIn% except)
  }

  # Subset the data
  xSubset <- terra::subset(x, xIndex)

  # Return the layers requested
  return(xSubset)
}
