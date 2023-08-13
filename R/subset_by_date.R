subset_by_date <- function(x,
                           dates = NULL,
                           dateList = NULL) {
  #' Subset a SpatRaster based on the layers' date
  #'
  #' @description Easily select only layers of a SpatRaster that are on specific
  #'   dates. This function is distinct to `subset_by_day()` as it considers the
  #'   full date, not just the day. It is usually quicker to chain together the
  #'   "subset_by_x" functions unless an unusual combination of dates are
  #'   required. See the examples.
  #'
  #'   The dates can be input either as a vector of specific dates using the
  #'   "dates" argument; or as a list of vectors containing the start and end
  #'   dates for longer extended periods. All dates need to be entered in
  #'   YYYY-MM-DD format (e.g. "2019-12-31").
  #'
  #' @param x SpatRaster: The data to subset. Can be either a string, in which
  #'   case it is interpreted as a filePath and read in, or an existing
  #'   SpatRaster.
  #' @param dates A vector of specific dates to return. Must be in the format
  #'   YYYY-MM-DD.
  #' @param dateList A list of vectors, with the first value in each vector
  #'   indicating when the period begins, and the second when the period ends.
  #'
  #' @examples
  #' \dontrun{
  #'   x <- terra::rast("fileName.nc")
  #'
  #'   # For specific dates, use the "dates" argument:
  #'   xSubset1 <- subset_by_date(x, dates = c("1991-12-01", "1994-12-19",
  #'                                           "2003-04-17", "2012-09-22"))
  #'
  #'   # For extended periods, use the "dateList" argument:
  #'   xSubset2 <- subset_by_date(x, dateList = list(c("1991-12-01", "1991-12-05"),
  #'                                                 c("1993-11-15", "1993-12-15")))
  #'
  #'   # However, it can be easier to chain other "subset_by_x" functions:
  #'   xSubset3 <- subset_by_year(x, 1991) |>
  #'               subset_by_month(12)
  #'
  #'   # instead of using subset_by_date:
  #'   xSubset4 <- subset_by_date(x, dateList = list(c("1991-12-01", "1991-12-31")
  #' }
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Check that dates have been requested!
  if (is.null(dates[1]) & is.null(dateList[1])) {
    stop("Please select some dates!")
  }
  if (!is.null(dates[1]) & !is.null(dateList[1])) {
    stop("Please use either the dates or dateList argument, not both.")
  }

  # Format the dates as necessary - account for using dates or dateList argument
  # if (is.null(dates[1])) {
  if (!is.null(dateList)) {
    # If using the dateList argument
    dates <- c() # preallocate

    # extend date periods and add
    for (ii in seq_along(dateList)) {
      iiDates <- as.Date(dateList[[ii]][1]):as.Date(dateList[[ii]][2])
      dates   <- c(dates, iiDates)
    }
    dates <- as.Date(dates, "1970-01-01") # format as dates
  }

  # Get all dates of the input
  xDates <- get_terra_dates(x, australSplit = NULL)

  # Preallocate to later subset with
  dateIndex <- rep(NA, length(dates))

  # Identify which layers need subsetting
  for (ii in seq_along(dates)) {
    # Prep the ii data
    iiDate <- dates[[ii]]
    iiYear  <- format(as.Date(iiDate), "%Y") |> as.numeric()
    iiMonth <- format(as.Date(iiDate), "%m") |> as.numeric()
    iiDay   <- format(as.Date(iiDate), "%d") |> as.numeric()

    # Retrieve & store the rownumber of the correct date layer
    dateIndex[ii] <- xDates[xDates$year == iiYear &
                              xDates$month == iiMonth &
                              xDates$day == iiDay, ] |>
      rownames() |>  # these should be numbers
      as.numeric()
  }

  # Subset the data
  xSubset <- terra::subset(x, dateIndex)

  return(xSubset)
}
