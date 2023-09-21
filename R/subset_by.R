subset_by <- function(x, type, exact = NULL,
                      before = NULL, after = NULL,
                      except = NULL,
                      australSplit = 3) {
  #' Subset a SpatRaster based on the layers' dates
  #'
  #' @description Easily select layers of a SpatRaster depending on their date
  #'   (and time, if present). This was designed as an internal function that
  #'   the other `subset_by_x` functions are based on but is also available on
  #'   its own.
  #'
  #' @param x SpatRaster: The data to subset. It can be either a string, in
  #'   which case it is interpreted as a filepath and read in using
  #'   [terra::rast()]; or an existing SpatRaster.
  #'
  #' @param type Which time/date type should the data be subset based on?
  #'   Options are (example using a date of 2019-11-29 and time of 12:30):
  #'   ```
  #'   - "date"          2019-11-29
  #'   - "year"          2019
  #'   - "month"         11
  #'   - "day"           29
  #'   - "monthDay"      Nov-29
  #'   - "summer"        2020
  #'   - "hour"          12
  #'   - "minute"        30
  #' ```
  #' @param exact vector: Which data should be included in the returned
  #'   SpatRaster? Only data that occurs on these exact dates / times is
  #'   returned. Cannot be used in conjunction with the 'before', 'after' or
  #'   'except' arguments.
  #'
  #' @param before Only data that occurs before this value is returned. Cannot
  #'   be used in conjunction with the exact values, 'after' or 'except'
  #'   arguments.
  #'
  #' @param after Only data that occurs after this value is returned. Cannot be
  #'   used in conjunction with the exact values, 'before' or 'except'
  #'   arguments.
  #'
  #' @param except vector: Which data should be excluded from the SpatRaster?
  #'   Cannot be used in conjunction with the exact values, 'after' or 'before'
  #'   arguments.
  #'
  #' @param australSplit numeric: If not FALSE, create an additional column for
  #'   the austral summer / year. For example, December 1991 and January 1992
  #'   often need to be considered as part of summer 1991/1992 in the southern
  #'   hemisphere rather than in their respective years.
  #'
  #'   The default value is 3, which means that all months *AFTER* March (i.e.
  #'   month 3) are considered as part of the following austral summer / year
  #'   (i.e. April 1991 -- March 1992 are all in austral summer / year 1992).
  #'
  #'   Swap this value accordingly: e.g. setting it as 4 means May 1991 -- April
  #'   1992 are all austral summer / year 1992.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Guard: only one of the options can be used at once
  check_if_null(exact, before, after, except,
                stopIfNoNull = TRUE, noNullMessage = "No dates selected!")

  # Handle if x is a filename
  x <- return_SpatRaster(x)

  # Get dates of each layer
  xDates <- get_date_info(x, australSplit = australSplit)

  # Identify layers
  if (!is.null(exact)) {
    xIndex <- which(xDates[[type]] %in% exact)
  } else if (!is.null(before)) {
    xIndex <- which(xDates[[type]] < before)
  } else if (!is.null(after)) {
    xIndex <- which(xDates[[type]] > after)
  } else if (!is.null(except)) {
    xIndex <- which(xDates[[type]] %notIn% except)
  }

  # Subset the data
  xSubset <- terra::subset(x, xIndex)

  # Return the layers requested
  return(xSubset)
}
