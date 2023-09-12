
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
