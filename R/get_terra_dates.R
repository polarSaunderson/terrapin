get_terra_dates <- function(x,
                            australSplit = 3) {
  #' Return a data frame of layer dates for a SpatRaster; useful for subsetting
  #'
  #' @description Often it is necessary to select only a subset of layers in a
  #'   dataset based on their date. For example, only layers in December, or in
  #'   2017. This function creates a data frame with the necessary date
  #'   information, which allows the correct layers to be identified and
  #'   filtered. Only works with SpatRasters from the `terra` package.
  #'
  #' @param x SpatRaster: The dataset.
  #' @param australSplit numeric: If not FALSE, create an additional column for
  #'   the austral summer. For example, December 1991 and January 1992 often
  #'   need to be considered as part of summer 1991/1992 rather than in their
  #'   respective years. The default value is 3, which means that all months
  #'   *AFTER* March (month 3) are considered as part of the following summer
  #'   (i.e. April 1991 -- March 1992 are all in summer 1992). Swap this value
  #'   according: setting it as 4 means May 1991 -- April 1992 are all summer
  #'   1992.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Get dates
  dates   <- terra::time(x)
  naDates <- is.na(dates)

  # Preallocate
  year     <- rep(NA, length(dates))
  month    <- rep(NA, length(dates))
  monthDay <- rep(NA, length(dates))
  day      <- rep(NA, length(dates))

  # Format dates into a dataframe
  if (sum(!naDates) > 0) {
    year[!naDates]     <- format(dates[!naDates], "%Y") |> as.numeric()
    month[!naDates]    <- format(dates[!naDates], "%m") |> as.numeric()
    monthDay[!naDates] <- format(dates[!naDates], "%m-%d")
    day[!naDates]      <- format(dates[!naDates], "%d") |> as.numeric()
  }
  rasterDates <- data.frame(year, month, day, monthDay)

  # Austral summers
  if (!is.null(australSplit)) {
    if (australSplit %in% 1:12) {
      summer <- year
      summer[month > australSplit] <- summer[month > australSplit] + 1
      rasterDates$summer <- summer
    }
  }

  return(rasterDates)
}
