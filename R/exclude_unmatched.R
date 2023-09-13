exclude_unmatched_months <- function(x,
                                     australSplit = FALSE,
                                     printClarity = FALSE) {
  #' Subset a SpatRaster to exclude months not found each year / summer
  #'
  #' @inherit exclude_incomplete_years
  #' @inheritParams exclude_incomplete_summers
  #'
  #' @export

  # Code ----------------------------------------------------------------------!
  # Just return if null; useful in other function calls
  if (is.null(australSplit)) {
    return(x)
  }

  # Basic date information
  xDates     <- get_date_info(x, australSplit = australSplit)
  xMonths    <- xDates$month |> unique() |> sort()
  incMonths  <- c()

  # exclude Unmatched Months
  # print("Removing unmatched months")
  if (isFALSE(australSplit)) {
    xYears    <- xDates$year |> unique() |> sort()
    for (ii in xMonths) {
      iiData  <- xDates[which(xDates$month == ii), ]
      iiYears <- iiData$year |> unique() |> sort()
      if (length(iiYears) == length(xYears)) {
        if (sum(iiYears != xYears) == 0) {
          incMonths <- c(incMonths, ii)
        }
      }
    }
    messageBit <- "years"
  } else if (australSplit %in% c(1:12)) {
    xSummers    <- xDates$summer |> unique() |> sort()
    for (ii in xMonths) {
      iiData    <- xDates[which(xDates$month == ii), ]
      iiSummers <- iiData$summer |> unique() |> sort()
      if (length(iiSummers) == length(xSummers)) {
        if (sum(iiSummers != xSummers) == 0) {
          incMonths <- c(incMonths, ii)
        }
      }
    }
    messageBit <- "summers"
  } else {
    stop("Set 'australSplit' as either FALSE or a number from 1:12")
  }

  # Stop and throw warning if no data is suitable
  if (length(incMonths) == 0) {
    cat("\n")
    warning(paste("No", messageBit, "contain all months found in this dataset!\n"))
    return("No data returned by exclude_unmatched_months")
  }

  # Print out summary happening to help clarify what is happening
  if (printClarity) {
    cat("The following month/s have data in all the", messageBit,
        "found in this dataset:\n",
        paste(month.name[incMonths], "\n"), "\n")

    excMonths <- xMonths[match(xMonths, incMonths, nomatch = 0) == 0]

    if (length(excMonths) > 0) {
      cat("Data from the following months will be excluded:\n",
          paste(month.name[excMonths], "\n"), "\n")
    } else {
      cat("No data needs to be excluded!\n\n")
    }
  }

  # Subset data
  incIndex <- which(xDates$month %in% incMonths)
  incData  <- terra::subset(x, incIndex)
}


exclude_unmatched_days <- function(x,
                                   australSplit = FALSE,
                                   printClarity = FALSE) {
  #' Subset a SpatRaster to exclude dates not found each year / summer
  #'
  #' @inherit exclude_incomplete_years
  #' @inheritParams exclude_incomplete_summers
  #'
  #' @export

  # Code ----------------------------------------------------------------------!
  # Just return if null; useful in other function calls (eg. racmoD_climatology)
  if (is.null(australSplit)) {
    return(x)
  }

  # Basic date information
  xDates          <- get_date_info(x, australSplit = australSplit)
  xDates$monthDay <- handle_monthDays(xDates$monthDay, out = "mm-dd")
  xMonthDays   <- xDates$monthDay |> unique() |> sort()
  incMonthDays <- c()

  # exclude Unmatched Dates
  # print("Removing unmatched dates")
  if (isFALSE(australSplit)) {
    xYears    <- xDates$year |> unique() |> sort()
    for (ii in xMonthDays) {
      iiData  <- xDates[which(xDates$monthDay == ii), ]
      iiYears <- iiData$year |> unique() |> sort()
      if (length(iiYears) == length(xYears)) {
        if (sum(iiYears != xYears) == 0) {
          incMonthDays <- c(incMonthDays, ii)
        }
      }
    }
    messageBit <- "years"
  } else if (australSplit %in% c(1:12)) {
    xSummers    <- xDates$summer |> unique() |> sort()
    for (ii in xMonthDays) {
      iiData    <- xDates[which(xDates$monthDay == ii), ]
      iiSummers <- iiData$summer |> unique() |> sort()
      if (length(iiSummers) == length(xSummers)) {
        if (sum(iiSummers != xSummers) == 0) {
          incMonthDays <- c(incMonthDays, ii)
        }
      }
    }
    messageBit <- "summers"
  } else {
    stop("Set 'australSplit' as either FALSE or a number from 1:12")
  }

  # Stop and throw warning if no data is suitable
  if (length(incMonthDays) == 0) {
    cat("\n")
    warning(paste("No", messageBit, "contain all month-day combinations found",
                  "in this dataset!\n"))
    return("No data returned by exclude_unmatched_days")
  }

  # Print out summary happening to help clarify what is happening
  if (printClarity) {
    cat("The following month-day combinations have data in all the", messageBit,
        "found in this dataset:\n",
        paste(incMonthDays, "\n"), "\n")

    excMonthDays <- xMonthDays[match(xMonthDays, incMonthDays, nomatch = 0) == 0]

    if (length(excMonthDays) > 0) {
      cat("Data from the following month-day combinations will be excluded:\n",
          paste(excMonthDays, "\n"), "\n")
    } else {
      cat("No data needs to be excluded!\n\n")
    }
  }

  # Subset data
  incIndex <- which(xDates$monthDay %in% incMonthDays)
  incData  <- terra::subset(x, incIndex)
}

