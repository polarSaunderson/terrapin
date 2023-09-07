subset_by_monthDay <- function(x,
                               monthDays = NULL,
                               monthDayList = NULL,
                               excludeIncomplete = FALSE) {
  #' Subset a SpatRaster based on the layers' month-day
  #'
  #' @description Easily select only layers of a SpatRaster that are on certain
  #'   month-days (i.e. dates ignoring the year).
  #'
  #'   As an example, return only the layers that fall on the 1st, 2nd or 3rd of
  #'   December for every year in the dataset.
  #'
  #' @inheritParams subset_by_month
  #' @param monthDays Used for specific dates. A vector of monthDays, best
  #'   formatted as "Jan-14" or "14-Jan". Fed directly into [handle_monthDays()]
  #'   to identify and reformat the month-day for matching against the
  #'   SpatRaster dates.
  #'
  #'   As an example, using a vector of `c("Jan-01", "7 Feb", "15/Mar)` will
  #'   find any layers that occur on the 1st of January or the 7th of February
  #'   or the 15th March in the 'x' dataset, regardless of the layers' year.
  #'
  #'   **Note:** Something like "01/02" will fail. It is ambiguous.
  #'
  #' @param monthDayList Used for extended periods of dates. Input must be a
  #'   list of vectors, with the first value in each vector indicating when the
  #'   period begins, and the second when the period ends.
  #'
  #'   For example, `list(c("Jan-02", "Jan-04"), c("Feb-07", "Feb-09"))` will
  #'   return all layers that occur on the 2nd, 3rd or 4th January, or on the
  #'   7th, 8th or 9th of February in the dataset, regardless of the layers'
  #'   year.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Check that some dates have been requested!
  if (is.null(monthDays[1]) & is.null(monthDayList[1])) {
    stop("No dates selected!")
  }
  if (!is.null(monthDays[1]) & !is.null(monthDayList[1])) {
    stop("Use either the monthDays or monthDayList argument, not both.")
  }

  # Handle different formats
  if (!is.null(monthDays)) {
    monthDays <- handle_monthDays(monthDays)
  }

  if (!is.null(monthDayList)) {
    monthDayList <- lapply(monthDayList, handle_monthDays)
  }

  # Handle if x is a filename
  if ("SpatRaster" %notIn% is(x)) {
    x <- terra::rast(x)
  }

  # Handle the various date entry options --------------------------------------
  if (!is.null(monthDayList)[1]) {
    # Handle if a c(start, end) vector is provided
    if ("list" %notIn% is(monthDayList)) {
      if (length(monthDayList) == 2) {
        monthDayList <- list(monthDayList)
      } else {
        stop("'monthDayList' must be either a single vector `c(start, end)`, ",
             "or a list of such vectors.")
      }
    } # else it must already be a list!

    # Separate the start and end dates
    startEach <- sapply(monthDayList, "[[", 1)
    endEach   <- sapply(monthDayList, "[[", 2)

    # Do the dates cross the new year?
    startMonth <- strsplit(startEach, "-") |> sapply("[[", 1) |> as.numeric()
    endMonth   <- strsplit(endEach, "-")   |> sapply("[[", 1) |> as.numeric()

    # Create fake dates (i.e. add a year to the month and day)
    # Allows start:end for the ranges
    fakeStart  <- ifelse(test = endMonth < startMonth,
                         yes  = "1979",
                         no   = "1980") |>
      paste(startEach, sep = "-")
    fakeEnd    <- paste("1980", endEach, sep = "-")

    # Get all dates in between the start and end dates (inclusive)
    fakeDates  <- c() # preallocate
    for (ii in seq_along(fakeStart)) {
      iiDates   <- as.Date(fakeStart[ii]):as.Date(fakeEnd[ii])
      fakeDates <- c(fakeDates, iiDates)
    }

    # Strip away the fake dates
    monthDays <- as.Date(fakeDates, "1970-01-01") |> strftime("%m-%d")
  }

  # Subset Data ----------------------------------------------------------------
  xDates <- get_terra_dates(x, australSplit = excludeIncomplete)

  # Which rows of the xDates monthDays match our requested monthDays?
  mdIndex <- which(xDates$monthDay %in% monthDays)

  # Subset
  xSubset <- terra::subset(x, mdIndex)

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
#
#   #
#   #
#
#   if ("list" %notIn% is(monthDays)) monthDays <- list(monthDays)
#
#   # Separate the start and end dates
#   startEach <- sapply(monthDays, "[[", 1)
#   endEach   <- sapply(monthDays, "[[", 2)
#
# # We need to know if the dates cross the new year for an austral summer split
# startMonth <- strsplit(startEach, "-") |> sapply("[[", 1) |> as.numeric()
# endMonth   <- strsplit(endEach,   "-") |> sapply("[[", 1) |> as.numeric()
#
# fakeStart <- ifelse(test = endMonth < startMonth,
#                     yes  = "1979",
#                     no   = "1980") |>
#   paste(startEach, sep = "-")
# fakeEnd  <- paste("1980", endEach, sep = "-")
#
# # Get all dates in between the starts and ends
# fakeDates <- c() # preallocate
# for (ii in seq_along(fakeStart)) {
#   iiDates  <- as.Date(fakeStart[ii]):as.Date(fakeEnd[ii])
#   fakeDates <- c(fakeDates, iiDates)
# }
#
# # Strip away the fakeYear part and keep as a string
# allMonthDays <- as.Date(fakeDates, "1970-01-01") |> strftime("%m-%d")
#
# # Subset
# xDates <- get_terra_dates(tstIn)
# x <- xDates$monthDay
#
# index <- which(xDates$monthDay %in% allMonthDays)
#
# xDates[index, ]
#
#
# }
