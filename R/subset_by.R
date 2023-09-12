subset_by <- function(x, type, exact = NULL,
                      before = NULL, after = NULL,
                      between = NULL, except = NULL, australSplit = 3) {
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
  #'   one of these 5 arguments can be used at once.
  #' @param before numeric: The maximum value that the 'type' value can have.
  #'   Only one of these 5 arguments can be used at once.
  #' @param after numeric: The minimum value that the 'type' value can have.
  #'   Only one of these 5 arguments can be used at once.
  #' @param between vector: The minimum and maximum values that the 'type' value
  #'   can have; inclusive.
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
  nullCount <- 0
  if (is.null(exact)) nullCount <- nullCount + 1
  if (is.null(before)) nullCount <- nullCount + 1
  if (is.null(after)) nullCount <- nullCount + 1
  if (is.null(between[1])) nullCount <- nullCount + 1
  if (is.null(except)) nullCount <- nullCount + 1
  if (nullCount > 4) {
    stop("Only one condition can be used at once")
  } else if (nullCount == 5) {
    warning("No conditions selected, returning the input")
    return(x)
  }

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
}


subset_by_year <- function(x, years = NULL,
                           before = NULL, after = NULL,
                           between = NULL, except = NULL) {
  #' Subset a SpatRaster based on the layers' year
  #'
  #' @description Easily select only the layers of a SpatRaster depending on
  #'   only the layers' year. For example, only data in 1991, or only data after
  #'   1999.
  #'
  #' @inheritParams subset_by
  #' @param years Which year/s to return? Use this argument for exact matches
  #'   (e.g. c(1983:1986, 1991:1992), otherwise leave this as NULL (the default)
  #'   and use one of the other arguments.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  x <- subset_by(x, type = "year", exact = years,
                 before = before, after = after,
                 between = between, except = except)
  return(x)
}

subset_by_summer <- function(x, summers = NULL,
                             before = NULL, after = NULL,
                             between = NULL, except = NULL) {
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
  x <- subset_by(x, type = "summer", exact = summers,
                 before = before, after = after,
                 between = between, except = except)
  return(x)
}

subset_by_hour <- function(x, hours = NULL,
                           before = NULL, after = NULL,
                           between = NULL, except = NULL) {
  #' Subset a SpatRaster based on the layers' hour
  #'
  #' @description Easily select only the layers of a SpatRaster depending on
  #'   only the layers' hour. For example, only data at 12:00, or only data
  #'   after 17:00. Works only on the hour (ignores minutes!) and if time
  #'   information is available in the 'x' raster.
  #'
  #' @inheritParams subset_by
  #' @param hours Which hour/s to return? Use this argument for exact matches
  #'   (e.g. c(12:17, 21-22)), otherwise leave this as NULL (the default) and
  #'   use one of the other arguments.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  x <- subset_by(x, type = "hour", exact = hours,
                 before = before, after = after,
                 between = between, except = except)
  return(x)
}

subset_by_minute <- function(x, minutes = NULL,
                             before = NULL, after = NULL,
                             between = NULL, except = NULL) {
  #' Subset a SpatRaster based on the layers' minute
  #'
  #' @description Easily select only the layers of a SpatRaster depending on
  #'   only the layers' minute. For example, only data at quarter past the hour
  #'   (15) or only data in the last half of an hour (>30). Works only on the
  #'   minute (ignores hours!) and if time information is available in the 'x'
  #'   raster.
  #'
  #' @inheritParams subset_by
  #' @param hours Which minute/s to return? Use this argument for exact matches
  #'   (e.g. c(0:15, 30:40)), otherwise leave this as NULL (the default) and use
  #'   one of the other arguments.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  x <- subset_by(x, type = "minute", exact = minutes,
                 before = before, after = after,
                 between = between, except = except)
  return(x)
}
