subset_by_date <- function(x,
                           dates = NULL,
                           periods = NULL) {
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
  #' @param dates Used for specific dates. Input must be a vector of specific
  #'   dates to return, each in the format "YYYY-MM-DD".
  #'
  #'   For example, c("2019-01-01", "2019-01-03", "2011-03-08") will return 3
  #'   layers (3 layers assuming those dates are available in the dataset and
  #'   each date only appears once; if not, all instances of a date will be
  #'   returned; if no dates match, an empty SpatRaster is returned).
  #'
  #' @param periods Used for extended periods rather than individual dates.
  #'   Input must be a vector, with the first value indicating when the period
  #'   begins and the second when the period ends. For multiple periods, use a
  #'   list of such vectors, for example:
  #'      ```
  #'      list(c("2019-01-01", "2019-01-03"),
  #'           c("2011-03-08", "2011-03-11"))
  #'      ````
  #'   The above will return the layers for 1st, 2nd and 3rd of January 2019,
  #'   and the 8th, 9th, 10th and 11th of March 2011.
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
    stop("No dates selected!")
  }
  if (!is.null(dates[1]) & !is.null(dateList[1])) {
    stop("Please use either the dates or periods argument, not both.")
  }

  # Handle if x is a filename
  if ("SpatRaster" %notIn% methods::is(x)) {
    x <- terra::rast(x)
  }

  # Format the dates as necessary - account for using dates or dateList argument
  if (!is.null(dateList)) {
    # Handle if a single vector was supplied
    if (!is.list(dateList)) dateList <- list(dateList)

    # If using the dateList argument
    dates <- c() # preallocate

    # extend date periods and add
    for (ii in seq_along(dateList)) {
      iiDates <- as.Date(dateList[[ii]][1]):as.Date(dateList[[ii]][2])
      dates   <- c(dates, iiDates)
    }
    dates <- as.Date(dates, "1970-01-01") |>
      as.character()                           # format as string of dates
  }

  # Get all dates of the input
  xDates <- get_terra_dates(x, australSplit = NULL)

  # Which rows of the xDates date match our requested dates?
  dateIndex <- which(xDates$date %in% dates)

  # Subset
  xSubset <- terra::subset(x, dateIndex)

  return(xSubset)
}
