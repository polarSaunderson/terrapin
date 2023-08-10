exclude_incomplete_years <- function(x,
                                     daily,
                                     printClarity = FALSE) {
  #' Subset a SpatRaster to exclude layers in years with incomplete data
  #'
  #' @description The 4 `exclude_x` functions are easiest to explain with
  #'   examples. Be very careful and deliberate with these functions to know
  #'   exactly what is being excluded.
  #'
  #'   Take a dataset containing monthly data for Jan-Apr in 1980 and 1981,
  #'   Jan--Mar in 1982, and Jan--Feb in 1983. If mean values were calculated on
  #'   a monthly basis, the January and February values would be calculated from
  #'   4 layers each (i.e. 1980, 1981, 1982, and 1983), but the March value
  #'   would be from 3 layers (1980, 1981, 1982), and the April data from just
  #'   two layers (1980, 1981). The `exclude_x` functions can help in a few ways
  #'   to wrangle the data before running the means.
  #'
  #'   ## Example 1
  #'   Using `exclude_incomplete_years`, the 1982 and 1983 data is
  #'   excluded. These two years do not contain all of the months found in the
  #'   data (i.e. Jan, Feb, Mar, Apr): neither 1982 or 1983 have April data, and
  #'   1983 does not have March data either. Only data layers in years in which
  #'   all of the months in the dataset (i.e. Jan--April) occur are retained:
  #'   here, that would be 1980 and 1981, which both have data for Jan--Apr.
  #'
  #'   The term "incomplete_years" is therefore relative; it is defined by the
  #'   data fed in. A year in this function is defined with calendar dates (i.e.
  #'   January through December); for the equivalent with austral years /
  #'   summers, use `exclude_incomplete_summers`.
  #'
  #'   This example is based on a monthly resolution (i.e. January, not e.g.
  #'   January 16th). A monthly resolution in this function is particularly
  #'   useful for monthly resolution data - all data layers in terra still
  #'   require a full date, but the day part is essentially an arbitrary number
  #'   (e.g. it could refer to the midpoint or end of the month, which depend on
  #'   the length of the month). Set the `daily` argument as TRUE to use this
  #'   function with daily resolution data (i.e. when the day part of the date
  #'   is important). If daily resolution, 29th February is ignored if missing,
  #'   and years/summers without it are still included.
  #'
  #'   ## Example 2
  #'   Taking the same initial dataset, and using
  #'   `exclude_unmatched_months`, the March and April data is excluded. These
  #'   two months do not have data in all of the years found in the data (i.e.
  #'   1980, 1981, 1982, 1983): April data doesn't exist in either 1982 or 1983,
  #'   and March data doesn't exist in 1982. Only data layers in months found in
  #'   all years of the dataset (i.e. 1980:1983) are retained: here, that would
  #'   be January and February, both of which have data in 1980:1983.
  #'
  #'   For the equivalent function, but using both the day and month part of the
  #'   date, use `exclude_unmatched_days`. Both of the `exclude_unmatched`
  #'   functions can be run for a calendar year ('australSplit = NULL') or for
  #'   an austral year / summer.
  #'
  #' @param x The SpatRaster data to subset.
  #' @param daily LOGICAL: If TRUE, the day and month part of the date are used
  #'   when considering which dates are present each year; if FALSE, only the
  #'   month is considered.
  #' @param printClarity LOGICAL: If TRUE, print out an explanation when the
  #'   function runs to clarify which data is being excluded and which included.
  #'
  #' @seealso exclude_incomplete_years
  #' @seealso exclude_incomplete_summers
  #' @seealso exclude_unmatched_months
  #' @seealso exclude_unmatched_days
  #' @seealso subset_by_year
  #' @seealso subset_by_summer
  #' @seealso subset_by_month
  #' @seealso subset_by_day
  #'
  #' @examples
  #'   \dontrun{
  #'     x <- terra::rast("xData.nc")
  #'
  #'     xComplete1 <- exclude_incomplete_years(x, daily = FALSE,
  #'                                            printClarity = TRUE)
  #'     xComplete2 <- exclude_incomplete_summers(x, daily = FALSE,
  #'                                              printClarity = TRUE)
  #'   }
  #'
  #' @export

  # Code ----------------------------------------------------------------------!
  # Basic date information
  xDates     <- get_terra_dates(x, australSplit = NULL)
  xYears     <- xDates$year |> unique() |> sort()
  incYears   <- c()

  # Exclude Incomplete Years
  # print("removing years")
  if (isTRUE(daily)) {
    # All unique month-days in the data; ignore 29th February
    xMonthDays <- xDates$monthDay |> unique() |> sort()
    if ("02-29" %in% xMonthDays) {
      xMonthDays <- xMonthDays[-which(xMonthDays == "02-29")]
    }
    for (ii in xYears) {
      iiData      <- xDates[which(xDates$year == ii), ]
      iiMonthDays <- iiData$monthDay |> unique() |> sort()
      # Ignore 29th February here too
      if ("02-29" %in% iiMonthDays) {
        iiMonthDays <- iiMonthDays[-which(iiMonthDays == "02-29")]
      }
      if (length(iiMonthDays) == length(xMonthDays)) {
        if (sum(iiMonthDays != xMonthDays) == 0) {
          incYears <- c(incYears, ii)
        }
      }
    }
    messageBit <- "month-day combination"
  } else if (isFALSE(daily)) {
    xMonths    <- xDates$month |> unique() |> sort()
    for (ii in xYears) {
      iiData   <- xDates[which(xDates$year == ii), ]
      iiMonths <- iiData$month |> unique() |> sort()
      # print(iiData)
      if (length(iiMonths) == length(xMonths)) {
        if (sum(iiMonths != xMonths) == 0) {
          incYears <- c(incYears, ii)
        }
      }
    }
    messageBit <- "month"
  }

  # Stop and throw warning if no data is suitable
  if (length(incYears) == 0) {
    cat("\n")
    warning(paste("No", messageBit, "is found in all years of this dataset!\n"))
    return("No data returned by exclude_incomplete_years")
  }

  # Print out summary happening to help clarify what is happening
  if (printClarity) {
    cat("\nThe following years have data for each", messageBit,
        "of this dataset:\n",
        paste(incYears, "\n"))

    excYears <- xYears[match(xYears, incYears, nomatch = 0) == 0]
    if (length(excYears) > 0) {
      cat("Data in the following years will be excluded:\n",
          paste(excYears, "\n"), "\n")
    } else {
      cat("No data needs to be excluded!\n\n")
    }
  }

  # Subset data
  incIndex <- which(xDates$year %in% incYears)
  incData  <- terra::subset(x, incIndex)
}

exclude_incomplete_summers <- function(x,
                                       daily,
                                       australSplit = 3,
                                       printClarity = FALSE) {
  #' Subset a SpatRaster to exclude layers in years with incomplete data
  #'
  #' @inherit exclude_incomplete_years description
  #' @inheritParams exclude_incomplete_years
  #' @inheritParams get_terra_dates
  #'
  #' @export

  # Code ----------------------------------------------------------------------!
  # Basic date information
  xDates     <- get_terra_dates(x, australSplit = australSplit)
  xSummers   <- xDates$summer |> unique() |> sort()
  xMonths    <- xDates$month |> unique() |> sort()
  xMonthDays <- xDates$monthDay |> unique() |> sort()
  incSummers <- c()

  # exclude Incomplete Summers
  # print("removing summers")
  if (isTRUE(daily)) {
    for (ii in xSummers) {
      iiData      <- xDates[which(xDates$summer == ii), ]
      iiMonthDays <- iiData$monthDay |> unique() |> sort()
      # print(iiData)
      if (length(iiMonthDays) == length(xMonthDays)) {
        if (sum(iiMonthDays != xMonthDays) == 0) {
          incSummers <- c(incSummers, ii)
        }
      }
    }
    messageBit <- "month-day combination"
  } else if (isFALSE(daily)) {
    for (ii in xSummers) {
      iiData   <- xDates[which(xDates$summer == ii), ]
      iiMonths <- iiData$month |> unique() |> sort()
      # print(iiData)
      if (length(iiMonths) == length(xMonths)) {
        if (sum(iiMonths != xMonths) == 0) {
          incSummers <- c(incSummers, ii)
        }
      }
    }
    messageBit <- "month"
  }

  # Stop and throw warning if no data is suitable
  if (length(incYears) == 0) {
    cat("\n")
    warning(paste("No", messageBit, "is found in all summers of this dataset!\n"))
    return("No data returned by exclude_incomplete_summers")
  }

  # Print out summary happening to help clarify what is happening
  if (printClarity) {
    cat("\nThe following summers have data for each", messageBit,
        "of this dataset:\n",
        paste(incSummers, "\n"))

    excSummers <- xSummers[match(xSummers, incSummers, nomatch = 0) == 0]
    if (length(excSummers) > 0) {
      cat("Data in the following summers will be excluded:\n",
          paste(excSummers, "\n"), "\n")
    } else {
      cat("No data needs to be excluded!\n\n")
    }
  }

  # Subset data
  incIndex <- which(xDates$summer %in% incSummers)
  incData  <- terra::subset(x, incIndex)
}
