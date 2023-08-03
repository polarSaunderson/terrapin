remove_incomplete_austral_summers <- function(x,
                                              daily = FALSE,
                                              australSplit = 3) {
  #' Subset a SpatRaster to exclude layers whose date is not matched in all
  #' austral summers
  #'
  #' @description This function is perhaps easiest to explain with an example,
  #'   first written in words and with the corresponding code examples below.
  #'   This function works as `remove_incomplete_years()` but uses austral years
  #'   / summers as the defining period to match; see that function for further
  #'   examples, but keep this distinction in mind. Be very careful and
  #'   deliberate when using this function because a lot of layers can be very
  #'   easily excluded.
  #'
  #'   *Example*: A dataset contains layers of December and January data in
  #'   1980--1983. However, the summer of 1979/1980 should have data layers for
  #'   December 1979 and January 1980, but the December 1979 data is not
  #'   available, so summer 1980 is incomplete. This function therefore excludes
  #'   the January 1980 data. Equally, the summer of 1984 should have the
  #'   December 1983 and January 1984 data, but the January 1984 data is not
  #'   available, so 1984 is an incomplete summer. This function therefore
  #'   excludes the December 1983 data. In the end, only the complete summers of
  #'   1981, 1982 and 1983 remain.
  #'
  #' @description If we subset based on summers, we can sometimes get instances
  #'   when the date is not found in all summers. For example, if we have data
  #'   for December and January each year from 1980 to 1983, we are missing the
  #'   December data of summer 1980, and the January data of summer 1983. With
  #'   this function, we would therefore remove the January 1980 data and the
  #'   December 1983 data as those summers are incomplete. It would leave us
  #'   only with the December and January data in summers 1981, 1982 and 1983.
  #'   It ignores if the 29th February is missing and will still return those
  #'   summers.
  #'
  #' @param x SpatRaster: The data to subset. Can be either a string, in which
  #'   case it is interpreted as a filePath and read in, or an existing
  #'   SpatRaster.
  #' @param daily BINARY: Should the the subsetting account for the day and the
  #'   month? If TRUE, the month and date must be matched in all summers for the
  #'   layer to be retained. If FALSE (the default), the layer is retained or
  #'   removed based only on the month part of the date. FALSE is useful because
  #'   monthly resolution data still requires a full date for each layer: the
  #'   day part of that date is essentially an arbitrary number, often the
  #'   midpoint or end of the month which depends on the length of the month.
  #'   See examples. If TRUE (i.e. it is daily resolution), 29th February is
  #'   ignored when missing, and those summers are still returned.
  #' @param australSplit numeric: Which is the last month included in an austral
  #'   summer before the new austral year begins? The default value is 3, which
  #'   means that all months *AFTER* March are considered as part of the
  #'   following summer (i.e. April 1991 -- March 1992 are all in 1992). Swap
  #'   this value according: setting it as 4 means May 1991 -- April 1992 are
  #'   all 1992.
  #'
  #' @seealso subset_by_summer()
  #' @seealso remove_incomplete_years()
  #'
  #' @export

  # Code ----------------------------------------------------------------------!
  # Basic date information
  xDates     <- get_terra_dates(x, austral = australSplit)
  xSummers   <- xDates$summer |> unique()
  incSummers <- c()                                 # preallocate


  # Establish which dates to include
  if (isFALSE(daily)) {
    # Option 1: Which summers contain all input *MONTHS*?
    xMonths    <- xDates$month |> unique() |> sort()  # all months in x

    # Which summers have all data?
    for (ii in xSummers) {
      iiData   <- xDates[which(xDates$summer == ii), ]
      iiMonths <- unique(iiData$month) |> sort()  # unique months in summer ii
      if (length(iiMonths) == length(xMonths)) {  # if unique month counts match
        if (sum(xMonths != iiMonths) == 0) {      # if months are the same
          incSummers <- c(incSummers, ii)           # then include this summer
        }
      }
    }
  } else if (isTRUE(daily)) {
    # Option 2: Which summers contain all input *DATES*?
    xDays   <- paste(sprintf("%02d", xDates$month),   # all day-month dates in x
                     sprintf("%02d", xDates$day), sep = "-") |>
      unique() |> sort()

    # Ignore February 29th
    if ("02-29" %in% xDays) {
      xDays <- xDays[-which(xDays == "02-29")]
    }

    # Which summers have all the data?
    for (ii in xSummers) {
      iiData   <- xDates[which(xDates$summer == ii), ]
      iiDays   <- paste(sprintf("%02d", iiData$month),
                        sprintf("%02d", iiData$day), sep = "-") |>
        unique() |> sort()

      # Remove 29th February if there - leap years mess up this whole approach
      if ("02-29" %in% iiDays) {
        iiDays <- iiDays[-which(iiDays == "02-29")]
      }

      if (length(iiDays) == length(xDays)) {    # if unique dates count match
        if (sum(iiDays != xDays) == 0) {        # if dates are the same
          incSummers <- c(incSummers, ii)          # then include this summer
        }
      }
    }
  }

  # Subset
  xSubset <- subset_by_summer(x, incSummers, australSplit)

  return(xSubset)
}
