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
  #' @param monthDays Which monthDay/s to return?  Used for specific monthDays.
  #'   Cannot be used in conjunction with the 'periods' argument.
  #'
  #'   Input must be a vector of specific monthDay/s, best formatted as "Jan-14"
  #'   or "14-Jan". The month must be explicit.
  #'
  #'   As an example, using a vector of `c("Jan-01", "7 Feb", "15/Mar)` will
  #'   find any layers that occur on the 1st of January or the 7th of February
  #'   or the 15th March in the input SpatRaster, regardless of the layers'
  #'   year.
  #'
  #'   **Note:** Something like "01/02" will fail. It is ambiguous. This
  #'   argument is fed directly into [terrapin::handle_monthDays()] to identify
  #'   and reformat the month-day, so see that function for details on accepted
  #'   inputs.
  #'
  #' @param periods Which monthDay/s to return? Used for extended periods rather
  #'   than individual monthDays. Cannot be used in conjunction with the
  #'   'monthDays' argument.
  #'
  #'   Input dates must be a vector, with the first value indicating when the
  #'   period begins and the second when the period ends.
  #'
  #'   For multiple periods, use a list of such vectors, for example:
  #'   ```
  #'   list(c("Jan-01", "Jan-01"),   # 1-3  Jan for every year in the SpatRaster
  #'        c("Mar-08", "Mar-11"))   # 8-11 Mar for every year in the SpatRaster
  #'   ```
  #'   The above will return data from the 1st, 2nd and 3rd of January, and the
  #'   8th, 9th, 10th and 11th of March, regardless of the year.
  #'
  #' @param except Which monthDay/s should be excluded from the SpatRaster? If a
  #'   vector, all dates are treated individually; for extended periods, use a
  #'   list of vectors as outlined in the 'periods' argument.
  #'
  #' @param excludeIncomplete Be careful using this argument! It can
  #'   dramatically affect the output of this function. Please read the
  #'   Explanation in [exclude_incomplete_years()] first.
  #'
  #'   If TRUE, the data is run through [exclude_incomplete_years()], and
  #'   only data from the years containing all requested monthDays are returned.
  #'
  #'   If numeric (between 1 and 12), the data is run through
  #'   [exclude_incomplete_summers], and the value is used as the 'australSplit'
  #'   argument. This returns only data from the austral summers that contain
  #'   all requested monthsDays.
  #'
  #'   If any other value (including the default FALSE), the
  #'   `exclude_incomplete_x` functions are skipped, and all layers matching the
  #'   'monthDays' argument are returned, regardless of the summer or year in
  #'   which they occur. See the examples.
  #'
  #' @export

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
      monthDays <- as.Date(fakeDates, "1970-01-01") |>
        handle_monthDays(out = "Jan-01")
      periods <- NULL
    } else if (!is.null(except)) {
      except <- as.Date(fakeDates, "1970-01-01") |>
        handle_monthDays(out = "Jan-01")
    }
  }

  # The monthDay doesn't work for > or < in format used in subset_by
  if (!is.null(before) | !is.null(after)) {
    # Handle if x is a filename
    x <- return_SpatRaster(x)

    # Dates
    xDates <- get_date_info(x)
    xMDays <- handle_monthDays(xDates$monthDay, out = "mm-dd")
    if (!is.null(before)) {
      before <- handle_monthDays(before, out = "mm-dd")
      xIndex <- which(xMDays < before)
    } else if (!is.null(after)) {
      after  <- handle_monthDays(after, out = "mm-dd")
      xIndex <- which(xMDays > after)
    }
    # Retrieve the layers of interest
    xSubset <- terra::subset(x, xIndex)
  } else {
    # Retrieve the layers of interest
    xSubset <- subset_by(x, type = "monthDay",
                         exact = monthDays,
                         before = before, after = after,
                         except = except)
  }

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
