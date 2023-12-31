
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
  #' @param dates Which date/s to return?  Used for specific dates Input must be
  #'   a vector of specific dates to return, with the dates formatted as
  #'   "YYYY-MM-DD", "YYYY-Jan-DD", "DD-MM-YYYY", or "DD-Jan-YYYY".
  #'
  #'   For example, c("2019-01-01", "03-01-2019", "2011-Mar-08") will return any
  #'   data layers on the 1st or 3rd January 2019, and on the 8th March 2011. If
  #'   no dates match, an empty SpatRaster is returned.
  #'
  #'   *NOTE:* Dates **cannot** be entered in the format "MM-DD-YYYY".
  #'   Internally, function identifies the year as the value with 4 digits, the
  #'   month as the middle value of the string, and the day as the remaining
  #'   part of the string.
  #'
  #' @param periods Which date/s to return? Used for extended periods rather
  #'   than individual dates. Cannot be used in conjunction with the 'dates'
  #'   argument.
  #'
  #'   Input dates must be a vector, with the first value indicating when the
  #'   period begins and the second when the period ends.
  #'
  #'   For multiple periods, use a list of such vectors, for example:
  #'   ```
  #'   list(c("2019-01-01", "2019-01-03"),     # 1-3  Jan 2019
  #'        c("2011-03-08", "2011-03-11"))     # 8-11 Mar 2011
  #'   ```
  #'
  #'   The above will return the layers for 1st, 2nd and 3rd of January 2019,
  #'   and the 8th, 9th, 10th and 11th of March 2011.
  #'
  #' @param except Which date/s should be excluded from the SpatRaster? If a
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
      periodStart <- handle_dates(periods[[ii]][1], out = "YYYY-MM-DD")
      periodEnd   <- handle_dates(periods[[ii]][2], out = "YYYY-MM-DD")
      iiDates     <- as.Date(periodStart):as.Date(periodEnd)
      dates       <- c(dates, iiDates)
    }

    # Format as string of dates
    dates <- as.Date(dates, "1970-01-01") |> as.character()
    periods <- NULL   # NULL is ready for subset_by, as only 1 non-null allowed

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
                       exact  = handle_dates(dates),
                       before = handle_dates(before),
                       after  = handle_dates(after),
                       except = handle_dates(except))

  return(xSubset)
}
