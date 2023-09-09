exclude_incomplete_years <- function(x,
                                     daily,
                                     printClarity = FALSE) {
  #' Subset a SpatRaster to exclude layers in years with incomplete data
  #'
  #' @description The `exclude_x` functions are easiest to explain with
  #'   theoretical examples. See "Explaining the Exclude Functions" below.
  #'
  #'   **___!!! WARNING !!!___**
  #'   Be very careful and deliberate with these functions to know exactly what
  #'   is being excluded.
  #'
  #' @details # Explaining the Exclude Functions
  #'
  #'   ## Premise
  #'   To explain these functions, take a dataset xData. xData is a SpatRaster
  #'   with 13 layers of data (shown here as a grid to highlight demonstrate
  #'   what happens).
  #'
  #'   ```
  #'       Theoretical Dataset xData
  #'       (each Mon YYYY is a layer in the SpatRaster)
  #'
  #'                        Jan       Feb       Mar       Apr
  #'       year 1980:     Jan 1980, Feb 1980, Mar 1980, Apr 1980
  #'       year 1981:     Jan 1981, Feb 1981, Mar 1981, Apr 1981
  #'       year 1982:     Jan 1982, Feb 1982, Mar 1982
  #'       year 1983:     Jan 1983, Feb 1983
  #'   ```
  #'
  #'   Imagine the following questions:
  #'
  #'   ```
  #'      - (A) Is the mean January value greater than the mean Feb value?
  #'      - (B) Is the mean January value greater than the mean Apr value?
  #'      - (C) Is the mean 1980 value greater than the mean 1981 value?
  #'      - (D) Is the mean 1980 value greater than the mean 1983 value?
  #'
  #'     Questions A is easy; there are 4 January datasets and
  #'     4 February datasets in the same years.
  #'
  #'     Question B is less clear. Does it make sense to compare
  #'     the mean of the Jan data in 4 years with the mean of
  #'     the Apr data in 2 years?
  #'
  #'     Question C is easy; the same 4 months in 1980 and in
  #'     1981 have data.
  #'
  #'     Question D is again less clear. Does it make sense to
  #'     compare the mean of 4 months in 1980 with the mean of
  #'     2 months in 1983?
  #'   ```
  #'
  #'   The `exclude_x` functions can help wrangle the data to answer these
  #'   questions. However, these functions are very happy to exclude data, so be
  #'   **extremely careful** using them and understand exactly what is being
  #'   excluded and whether that makes sense.
  #'
  #'   ## Example 1: [exclude_incomplete_years()] for Question B
  #'   **Result**: Using `exclude_incomplete_years`, the 1982 and 1983 data is
  #'   excluded.
  #'
  #'   ```
  #'       Theoretical Dataset xData
  #'       *AFTER* exclude_unmatched_months()
  #'
  #'                        Jan       Feb       Mar       Apr
  #'       year 1980:     Jan 1980, Feb 1980, Mar 1980, Apr 1980
  #'       year 1981:     Jan 1981, Feb 1981, Mar 1981, Apr 1981
  #'   ```
  #'
  #'   *note:*
  #'   With this function, we remove the incomplete rows in the gridded
  #'   representation of the xData.
  #'
  #'   **Why?** Our dataset has Jan, Feb, Mar and Apr data. However, there is only
  #'   data for all 4 of these months in 1980 and 1981; neither 1982 or 1983
  #'   have Apr data, and there is no Mar data in 1983 either. Therefore, the
  #'   1982 and 1983 years are "incomplete years", and excluded. The term
  #'   "incomplete years" is thus relative, being defined in relation to dataset
  #'   fed in.
  #'
  #'   For Question B, we can now just calculate the mean of all January values
  #'   (1980, 1981) and of all Apr values (1980, 1981) in the dataset, and
  #'   compare the values.
  #'
  #'   ## Example 2: [exclude_unmatched_months()] for Question D
  #'   **Result**: Using `exclude_unmatched_months`, the Mar and Apr data is
  #'   excluded.
  #'
  #'   ```
  #'       Theoretical Dataset xData
  #'       *AFTER* xData after exclude_unmatched_months()
  #'                        Jan       Feb
  #'       year 1980:     Jan 1980, Feb 1980
  #'       year 1981:     Jan 1981, Feb 1981
  #'       year 1982:     Jan 1982, Feb 1982
  #'       year 1983:     Jan 1983, Feb 1983
  #'   ```
  #'
  #'   *note:*
  #'   With this function, we remove the incomplete columns in the gridded
  #'   representation of the xData.
  #'
  #'   **Why?** The xData dataset has data in 4 months (Jan, Feb, Mar, Apr).
  #'   However, there is no Mar and Apr data available in each of the years
  #'   found in the dataset - there is no Apr data for 1982 or 1983, and no Mar
  #'   data in 1982 either. Therefore, we consider that the Mar and Apr data is
  #'   "unmatched" in all years of the dataset, and exclude these months.
  #'
  #'   For Question D, we can now mean all of the data in 1980 (Jan, Feb) and
  #'   mean all of the data in 1981 (Jan, Feb), and compare the values.
  #'
  #'   ## Temporal Resolution
  #'
  #'   The above examples are based on using monthly resolution data (i.e. Jan,
  #'   not Jan 1st, Jan 2nd, Jan 3rd...). For the [exclude_incomplete_years()]
  #'   function, it is therefore necessary to set the 'daily' argument as FALSE.
  #'   This is necessary because all SpatRaster layers must have a complete date
  #'   (i.e. YYYY-MM-DD), but for monthly data, the "DD" is essentially a
  #'   random number (e.g. the first day in the month, the midpoint, the end).
  #'   The DD part of the date should therefore be ignored.
  #'
  #'   However, for daily data, the "DD" part is vital, and 'daily' should be
  #'   set as TRUE.
  #'
  #'   When using [exclude_unmatched_months()], the monthly resolution is built
  #'   into the function name. The equivalent for daily data requires the
  #'   [exclude_unmatched_days()] function.
  #'
  #'   ## Years vs Austral Summers
  #'   The above examples were based on the gridded representation of xData
  #'   using years as the rows. Often in the southern hemisphere it makes more
  #'   sense to think of austral years, particularly in the summer where (e.g.)
  #'   Dec 1991 and Jan 1992 should be considered together as part of summer
  #'   1992.
  #'
  #'   ```
  #'       Theoretical Dataset sData
  #'       (each Mon YYYY is a layer in the SpatRaster)
  #'                          Nov       Dec       Jan       Feb
  #'       summer 1980:     Nov 1979, Dec 1979, Jan 1980, Feb 1980
  #'       summer 1981:     Nov 1980, Dec 1980, Jan 1981, Feb 1981
  #'       summer 1982:     Nov 1981, Dec 1981, Jan 1982, Feb 1982
  #'       summer 1983:     Nov 1982, Dec 1982, Jan 1983
  #'   ```
  #'
  #'   ## Example 3: `exclude_unmatched_months(australSplit = 3)`
  #'   **Result**: Using `exclude_unmatched_months(australSplit = 3)`, the Feb
  #'    data is excluded.
  #'
  #'   ```
  #'       Theoretical Dataset sData
  #'       *AFTER* exclude_unmatched_months(australSplit = 3)
  #'                          Nov       Dec       Jan
  #'       summer 1980:     Nov 1979, Dec 1979, Jan 1980
  #'       summer 1981:     Nov 1980, Dec 1980, Jan 1981
  #'       summer 1982:     Nov 1981, Dec 1981, Jan 1982
  #'       summer 1983:     Nov 1982, Dec 1982, Jan 1983
  #'   ```
  #'
  #'   *note:*
  #'   With this function, we remove the incomplete columns in the
  #'   gridded representation of the sData (when rows are summers).
  #'
  #'   *___!!! Beware !!!___*
  #'   If 'australSplit' had been set as FALSE (so the gridded
  #'   representation uses years as rows), all data would have been excluded:
  #'
  #'       - Nov and Dec are not found in 1983;
  #'       - Jan is not found in 1979;
  #'       - Feb is not found in 1979 or 1983.
  #'
  #'   Therefore all months would have been unmatched:
  #'
  #'   ```
  #'       Theoretical Dataset sData
  #'       *AFTER* sData after exclude_unmatched_months()
  #'
  #'       !!! EMPTY !!!
  #'   ```
  #'   This example also highlights how aggressively these functions want to
  #'   exclude data!
  #'
  #'   The 'australSplit' argument is also available within
  #'   [exclude_unmatched_days()].
  #'
  #'   ## Example 4 [exclude_incomplete_summers()]
  #'   **Result**: Using `exclude_incomplete_summer()`, the 1983 data is
  #'   excluded.
  #'
  #'   ```
  #'       Theoretical Dataset xData
  #'       *AFTER* sData after exclude_incomplete_summers()
  #'                          Nov       Dec       Jan       Feb
  #'       summer 1980:     Nov 1979, Dec 1979, Jan 1980, Feb 1980
  #'       summer 1981:     Nov 1980, Dec 1980, Jan 1981, Feb 1981
  #'       summer 1982:     Nov 1981, Dec 1981, Jan 1982, Feb 1982
  #'   ```
  #'
  #'   *note:* With this function, we remove the incomplete rows in the
  #'   gridded representation of the sData (when rows are summers).
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
  xDates     <- get_terra_dates(x, australSplit = FALSE)
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
  #' @inherit exclude_incomplete_years
  #'
  #' @param australSplit numeric: Which is the last month included in an austral
  #'   summer before the new austral year begins? The default value is 3, which
  #'   means that all months *AFTER* March are considered as part of the
  #'   following summer (i.e. April 1991 -- March 1992 are all in 1992). Swap
  #'   this value according: setting it as 4 means May 1991 -- April 1992 are
  #'   all 1992.
  #'
  #' @export

  # Code ----------------------------------------------------------------------!
  # Basic date information
  xDates     <- get_terra_dates(x, australSplit = australSplit)
  xSummers   <- xDates$summer |> unique() |> sort()
  incSummers <- c()

  # exclude Incomplete Summers
  # print("removing summers")
  if (isTRUE(daily)) {
    # All unique month-days in the data; ignore 29th February
    xMonthDays <- xDates$monthDay |> unique() |> sort()
    if ("02-29" %in% xMonthDays) {
      xMonthDays <- xMonthDays[-which(xMonthDays == "02-29")]
    }
    for (ii in xSummers) {
      iiData      <- xDates[which(xDates$summer == ii), ]
      iiMonthDays <- iiData$monthDay |> unique() |> sort()
      # Ignore 29th February here too
      if ("02-29" %in% iiMonthDays) {
        iiMonthDays <- iiMonthDays[-which(iiMonthDays == "02-29")]
      }
      # print(iiData)
      if (length(iiMonthDays) == length(xMonthDays)) {
        if (sum(iiMonthDays != xMonthDays) == 0) {
          incSummers <- c(incSummers, ii)
        }
      }
    }
    messageBit <- "month-day combination"
  } else if (isFALSE(daily)) {
    xMonths    <- xDates$month |> unique() |> sort()
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
  if (length(incSummers) == 0) {
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
