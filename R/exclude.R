exclude_incomplete_years <- function(x, daily, printClarity = FALSE) {

  # Code ----------------------------------------------------------------------!
  # Basic date information
  xDates  <- get_terra_dates(x, austral = NULL)
  xYears  <- xDates$year |> unique() |> sort()
  xMonths <- xDates$month |> unique() |> sort()
  xDays   <- xDates$monthDay |> unique() |> sort()

  # Remove Incomplete Years
  print("removing years")
  incYears <- c()

  if (isTRUE(daily)) {
    for (ii in xYears) {
      iiData <- xDates[which(xDates$year == ii), ]
      iiDays <- iiData$monthDay |> unique() |> sort()
      if (length(iiDays) == length(xDays)) {
        if (sum(iiDays != xDays) == 0) {
          incYears <- c(incYears, ii)
        }
      }
    }


    if (printClarity) {
      cat("The following years have data for each month-day of this dataset:\n",
          paste(incYears, "\n"))

      excYears <- xYears[match(xYears, incYears, nomatch = 0) == 0]
      if (length(excYears) > 0) {
        cat("Data in the following years will be excluded:\n",
            paste(excYears, "\n"))
      } else {
        cat("No data needs to be excluded!\n")
      }
    }


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

    if (printClarity) {
      cat("The following years have data for each month of this dataset:\n",
          paste(incYears, "\n"))

      excYears <- xYears[match(xYears, incYears, nomatch = 0) == 0]
      if (length(excYears) > 0) {
        cat("Data in the following years will be excluded:\n",
            paste(excYears, "\n"))
      } else {
        cat("No data needs to be excluded!\n")
      }
    }
  }

  # Subset data
  incIndex <- which(xDates$year %in% incYears)
  incData  <- terra::subset(x, incIndex)
}

# exclude_incomplete_summers <- function(x, daily) {}

# exclude_unmatched_months <- function(x, austral) {}

# exclude_unmatched_days <- function(x, austral) {}
