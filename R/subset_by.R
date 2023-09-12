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
  #'   (e.g. c(1983:1986, 1991:1992), otherwise leave this as NULL (the default)
  #'   and use one of the other arguments.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  xSubset <- subset_by(x, type = "year", exact = years,
                       before = before, after = after,
                       except = except)
  return(xSubset)
}

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
  #'   (e.g. c(1:4, 8), otherwise leave this as NULL (the default) and use one
  #'   of the other arguments. This argument is fed through [get_months()], so
  #'   can be input either as (e.g.) 12, "12", "Dec", "dec", "December", or
  #'   "december".
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


subset_by_hour <- function(x, hours = NULL,
                           before = NULL, after = NULL,
                           except = NULL) {
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
  #'   use one of the other arguments. Must be in the 24-hour format.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  xSubset <- subset_by(x, type = "hour",
                       exact = hours,
                       before = before, after = after,
                       except = except)
  return(xSubset)
}

subset_by_minute <- function(x, minutes = NULL,
                             before = NULL, after = NULL,
                             except = NULL) {
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
  xSubset <- subset_by(x, type = "minute",
                       exact = minutes,
                       before = before, after = after,
                       except = except)
  return(xSubset)
}


subset_by_date <- function(x, dates = NULL,
                           periods = NULL,
                           before = NULL,
                           after = NULL,
                           except = NULL) {
  #' Subset a SpatRaster based on the layers' date
  #'
  #' @description Easily select only layers of a SpatRaster that are on specific
  #'   dates. This function is distinct to `subset_by_day()` as it considers the
  #'   full date, not just the day. It is usually easier (and quicker) to chain
  #'   together the "subset_by_x" functions unless an unusual combination of
  #'   dates is required. See the examples.
  #'
  #'   The dates can be input either as a vector of specific dates using the
  #'   "dates" argument; or as a list of vectors containing the start and end
  #'   dates for longer extended 'periods'. All dates need to be entered in
  #'   YYYY-MM-DD format (e.g. "2019-12-31").
  #'
  #' @inheritParams subset_by
  #' @param dates Used for specific dates. Input must be a vector of specific
  #'   dates to return, each in the format "YYYY-MM-DD".
  #'
  #'   For example, c("2019-01-01", "2019-01-03", "2011-03-08") will return 3
  #'   layers (3 layers assuming those dates are available in the dataset and
  #'   each date only appears once; if not, all instances of a date will be
  #'   returned; if no dates match, an empty SpatRaster is returned).
  #' @param periods Used for extended periods rather than individual dates.
  #'   Input dates must be a vector, with the first value indicating when the
  #'   period begins and the second when the period ends. For multiple periods,
  #'   use a list of such vectors, for example:
  #'      ```
  #'      list(c("2019-01-01", "2019-01-03"),     # 1-3  Jan 2019
  #'           c("2011-03-08", "2011-03-11"))     # 8-11 Mar 2011
  #'      ````
  #'   The above will return the layers for 1st, 2nd and 3rd of January 2019,
  #'   and the 8th, 9th, 10th and 11th of March 2011.
  #' @param except Dates that should be removed from the 'x' SpatRaster. If a
  #'   vector, all dates are treated individually; for extended periods, use a
  #'   list of vectors as outlined in the 'periods' argument.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Guard: only one of the options can be used at once
  check_if_null(dates, periods, before, after, except,
                stopIfNoNull = TRUE, noNullMessage = "No dates selected!")

  # Format the input as necessary - account for using periods / except arguments
  if (!is.null(periods)) {
    # Handle if a single vector was supplied
    if (!is.list(periods)) periods <- list(periods)

    # If using the periods argument
    dates <- c() # preallocate

    # extend date periods and add
    for (ii in seq_along(periods)) {
      iiDates <- as.Date(periods[[ii]][1]):as.Date(periods[[ii]][2])
      dates   <- c(dates, iiDates)
    }
    dates <- as.Date(dates, "1970-01-01") |>
      as.character()                           # format as string of dates
  } else if (!is.null(except)) {
    if (is.list(except)) {              # if a list, find the full periods
      newExcept <- c() # preallocate

      # extend date periods and add
      for (ii in seq_along(except)) {
        iiDates <- as.Date(except[[ii]][1]):as.Date(except[[ii]][2])
        newExcept <- c(newExcept, iiDates)
      }
      except <- as.Date(newExcept, "1970-01-01") |>
        as.character()
    } # else it is a vector, and we want each date individually
  }

  # Retrieve correct data layers
  xSubset <- subset_by(x = x, type = "date",
                       exact = dates,
                       before = before, after = after,
                       except = except)
  return(xSubset)
}

subset_by_monthDay <- function(x,
                               monthDays = NULL,
                               periods = NULL,
                               excludeIncomplete = FALSE,
                               before = NULL,
                               after = NULL,
                               except = NULL) {
  #' Subset a SpatRaster based on the layers' month-day
  #'
  #' @description Easily select only layers of a SpatRaster that are on certain
  #'   month-days (i.e. dates which ignore the year).
  #'
  #'   As an example, return only the layers that fall on the 1st, 2nd or 3rd of
  #'   December for every year in the dataset.
  #'
  #' @inheritParams subset_by
  #' @param monthDays Used for specific dates. A vector of monthDays, best
  #'   formatted as "Jan-14" or "14-Jan". Fed directly into [handle_monthDays()]
  #'   to identify and reformat the month-day for matching against the 'x'
  #'   SpatRaster dates.
  #'
  #'   As an example, using a vector of `c("Jan-01", "7 Feb", "15/Mar)` will
  #'   find any layers that occur on the 1st of January or the 7th of February
  #'   or the 15th March in the 'x' dataset, regardless of the layers' year.
  #'
  #'   **Note:** Something like "01/02" will fail. It is ambiguous.
  #' @param periods Used for extended periods rather than individual dates.
  #'   Input dates must be a vector, with the first value indicating when the
  #'   period begins and the second when the period ends. For multiple periods,
  #'   use a list of such vectors, for example:
  #'      ```
  #'      list(c("Jan-01", "Jan-01"),     # 1-3  Jan of all years in x
  #'           c("Mar-08", "Mar-11"))     # 8-11 Mar of all years in x
  #'      ````
  #'   The above will return the layers for 1st, 2nd and 3rd of January, and the
  #'   8th, 9th, 10th and 11th of March, regardless of the year.
  #' @param except Dates that should be removed from the 'x' SpatRaster. If a
  #'   vector, all dates are treated individually; for extended periods, use a
  #'   list of vectors as outlined in the 'periods' argument.
  #'

  # Code -----------------------------------------------------------------------
  # Guard: only one of the options can be used at once
  check_if_null(monthDays, periods, before, after, except,
                stopIfNoNull = TRUE, noNullMessage = "No dates selected!")

  # Handle different formats
  if (!is.null(monthDays[1])) {
    monthDays <- handle_monthDays(monthDays, out = "Jan-01")
  } else if (!is.null(periods[1])) {
    if (!is.list(periods)) {
      periods <- list(periods)
    }
    periods <- lapply(periods,
                      FUN = handle_monthDays,
                      out = "Jan-01")
    toPrepare <- periods
  } else if (!is.null(before)) {
    before <- handle_monthDays(before, out = "Jan-01")
  } else if (!is.null(after)) {
    after <- handle_monthDays(after, out = "Jan-01")
  } else if (!is.null(except[1])) {
    if (is.list(except)) {
      except <- lapply(except,
                       FUN = handle_monthDays,
                       out = "Jan-01")
      toPrepare <- except
    } else {
      except <- handle_monthDays(except)
    }
  }

  # Handle if periods or except list were used as input
  if (exists("toPrepare")) {
    # Separate the start and end date
    startEach <- sapply(toPrepare, "[[", 1)
    endEach   <- sapply(toPrepare, "[[", 2)

    # Do the dates cross the new year?
    startMonth <- get_months(startEach, 1)
    endMonth   <- get_months(endEach, 1)

    # Create fake dates (i.e. add a year to the month and day)
    # Allows start:end for the ranges
    fakeStart  <- ifelse(test = endMonth < startMonth,
                         yes  = "1979",
                         no   = "1980") |>
      paste(handle_monthDays(startEach, "mm-dd"), sep = "-")
    fakeEnd    <- paste("1980", handle_monthDays(endEach, "mm-dd"), sep = "-")

    # Get all dates in between the start and end dates (inclusive)
    fakeDates  <- c() # preallocate
    for (ii in seq_along(fakeStart)) {
      iiDates   <- as.Date(fakeStart[ii]):as.Date(fakeEnd[ii])
      fakeDates <- c(fakeDates, iiDates)
    }

    # Strip away the fake dates
    if (!is.null(periods)) {
      periods <- as.Date(fakeDates, "1970-01-01") |>
        handle_monthDays(out = "Jan-01")
      cat2(periods)
    } else if (!is.null(except)) {
      except <- as.Date(fakeDates, "1970-01-01") |>
        handle_monthDays(out = "Jan-01")
    }
  }

  # Retrieve the layers of interest
  xSubset <- subset_by(x, type = "monthDay",
                       exact = monthDays,
                       before = before, after = after,
                       except = except)

  # Handle if removing
  if (excludeIncomplete %in% 1:12) {
    xSubset <- exclude_incomplete_summers(x = xSubset,
                                          daily = TRUE,
                                          australSplit = excludeIncomplete)
    # If all of these dates don't appear in a summer, remove any that do.
  } else if (isTRUE(excludeIncomplete)) {
    xSubset <- exclude_incomplete_years(x = xSubset,
                                        daily = TRUE)
    # If all of these dates don't appear in a year, remove any that do.
  }

  return(xSubset)
}
