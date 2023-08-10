exclude_incomplete_years <- function(x, daily, printClarity = FALSE) {

  # Code ----------------------------------------------------------------------!
  # Basic date information
  xDates     <- get_terra_dates(x, austral = NULL)
  xYears     <- xDates$year |> unique() |> sort()
  xMonths    <- xDates$month |> unique() |> sort()
  xMonthDays <- xDates$monthDay |> unique() |> sort()

  # Remove Incomplete Years
  # print("removing years")
  incYears <- c()

  if (isTRUE(daily)) {
    for (ii in xYears) {
      iiData      <- xDates[which(xDates$year == ii), ]
      iiMonthDays <- iiData$monthDay |> unique() |> sort()
      if (length(iiMonthDays) == length(xMonthDays)) {
        if (sum(iiMonthDays != xMonthDays) == 0) {
          incYears <- c(incYears, ii)
        }
      }
    }
    messageBit <- "month-day combination"
  } else if (isFALSE(daily)) {
    for (ii in xYears) {
      iiData   <- xDates[which(xDates$year == ii), ]
      iiMonths <- iiData$month |> unique() |> sort()
      if (length(iiMonths) == length(xMonths)) {
        if (sum(iiMonths != xMonths) == 0) {
          incYears <- c(incYears, ii)
        }
      }
    }
    messageBit <- "month"
  }

  # Print out summary happening to help clarify what is happening
  if (printClarity) {
    if (length(incYears) > 0) {
      cat("\nThe following years have data for each", messageBit,
          "of this dataset:\n",
          paste(incYears, "\n"))
      } else {
        cat("No", messageBit, "is found in all years of this dataset!\n")
        return("No data returned by exclude_incomplete_years")
      }

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


exclude_incomplete_summers <- function(x, daily,
                                       australSplit = 3,
                                       printClarity = FALSE) {

  # Code ----------------------------------------------------------------------!
  # Basic date information
  xDates     <- get_terra_dates(x, austral = australSplit)
  xSummers   <- xDates$summer |> unique() |> sort()
  xMonths    <- xDates$month |> unique() |> sort()
  xMonthDays <- xDates$monthDay |> unique() |> sort()

  # Remove Incomplete Summers
  print("removing summers")
  incSummers <- c()

  if (isTRUE(daily)) {
    for (ii in xSummers) {
      iiData      <- xDates[which(xDates$summer == ii), ]
      iiMonthDays <- iiData$monthDay |> unique() |> sort()
      print(iiData)
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
      print(iiData)
      if (length(iiMonths) == length(xMonths)) {
        if (sum(iiMonths != xMonths) == 0) {
          incSummers <- c(incSummers, ii)
        }
      }
    }
    messageBit <- "month"
  }

  # Print out summary happening to help clarify what is happening
  if (printClarity) {
    if (length(incSummers) > 0) {
      cat("\nThe following summers have data for each", messageBit,
          "of this dataset:\n",
          paste(incYears, "\n"))
      } else {
        cat("No", messageBit, "is found in all summers of this dataset!\n")
        return("No data returned by exclude_incomplete_summers")
      }

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

# exclude_incomplete_summers <- function(x, daily) {}

# exclude_unmatched_months <- function(x, austral) {}

# exclude_unmatched_days <- function(x, austral) {}


# exclude_incomplete_summers <- function(x, daily) {}

# exclude_unmatched_months <- function(x, austral) {}

# exclude_unmatched_days <- function(x, austral) {}
