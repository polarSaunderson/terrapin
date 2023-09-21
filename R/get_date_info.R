get_date_info <- function(x,
                          australSplit = 3) {
  #' Return a data frame of SpatRaster layer dates for subsetting
  #'
  #' @description The [get_date_info()] function is the basis of the `terrapin`
  #'   package. It simply creates a data frame of the time information stored in
  #'   a SpatRaster (each row is a SpatRaster layer), making it easy to subset
  #'   and/or exclude layers in the SpatRaster based on their date / time.
  #'
  #'   It is used in the `subset_by_x` functions, but can also be used in any
  #'   instance where it is useful to have the date / time in a dataframe.
  #'
  #'   This function is based on [terra::time()] and only works with SpatRasters
  #'   from the `terra` package. The layers **must** have dates for this
  #'   function to work; if the layers don't have associated times, the time
  #'   columns are simply not created.
  #'
  #' @param x SpatRaster: The dataset.
  #' @param australSplit numeric: If an integer between 1 and 12, an additional
  #'   column is created in the data frame to store the austral summer / year.
  #'
  #'   For example, in the Southern Hemisphere, December 1991 and January 1992
  #'   often need to be considered as part of summer 1991/1992 (here referred to
  #'   as summer 1992) rather than in their respective years.
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
  # Get date (and time?) information
  dateInfo <- terra::time(x)
  naDates  <- is.na(dateInfo)

  # Preallocate for dates
  date     <- rep(NA, length(dateInfo))
  year     <- rep(NA, length(dateInfo))
  month    <- rep(NA, length(dateInfo))
  monthDay <- rep(NA, length(dateInfo))
  day      <- rep(NA, length(dateInfo))

  # Format dates
  if (sum(!naDates) > 0) {
    date[!naDates]     <- format(dateInfo[!naDates], "%F")
    year[!naDates]     <- format(dateInfo[!naDates], "%Y") |> as.numeric()
    month[!naDates]    <- format(dateInfo[!naDates], "%m") |> as.numeric()
    monthDay[!naDates] <- format(dateInfo[!naDates], "%b-%d")
    day[!naDates]      <- format(dateInfo[!naDates], "%d") |> as.numeric()
  }

  # Combine as a dataframe for $ access
  rasterDates <- data.frame(date, year, month, day, monthDay)

  # Add austral summer column
  if (!is.null(australSplit)) {
    if (australSplit %in% 1:12) {
      summer <- year
      summer[month > australSplit] <- summer[month > australSplit] + 1
      rasterDates$summer <- summer
    }
  }

  # Time information
  if ("POSIXct" %in% methods::is(dateInfo[[1]]) |
      "POSIXt" %in% methods::is(dateInfo[[1]])) {
    # Preallocate
    time     <- rep(NA, length(dateInfo))
    hour     <- rep(NA, length(dateInfo))
    minute   <- rep(NA, length(dateInfo))
    dateTime <- rep(NA, length(dateInfo))

    # Format
    time[!naDates]     <- format(dateInfo[!naDates], "%H:%M")
    hour[!naDates]     <- format(dateInfo[!naDates], "%H") |> as.numeric()
    minute[!naDates]   <- format(dateInfo[!naDates], "%M") |> as.numeric()
    dateTime[!naDates] <- format(dateInfo[!naDates], "%F %H:%M")

    # Store
    rasterDates$time     <- time
    rasterDates$hour     <- hour
    rasterDates$minute   <- minute
    rasterDates$dateTime <- dateTime
  }

  return(rasterDates)
}
