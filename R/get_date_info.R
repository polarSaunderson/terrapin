get_date_info <- function(x,
                          australSplit = 3) {
  #' Return a data frame of SpatRaster layer dates for subsetting
  #'
  #' @description Often it is necessary to select only a subset of layers in a
  #'   dataset based on their date or time. For example, only layers in
  #'   December, only in 2017, or only in the afternoon. The date (and possibly
  #'   time) information is stored in the SpatRaster; this function creates a
  #'   simple data frame to hold the date information, thus allowing the correct
  #'   layers to be identified and filtered. This only works with SpatRasters
  #'   from the `terra` package, and they must have the date & time information
  #'   necessary. Any missing data is assigned a value of NA. If the data has
  #'   no time information, the time columns are excluded.
  #'
  #' @param x SpatRaster: The dataset.
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
  if ("POSIXct" %in% is(dateInfo[[1]]) | "POSIXt" %in% is(dateInfo[[1]])) {
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
