handle_dates <- function(x, out = "YYYY-MM-DD", outSep = "-") {
  #' Reformat date information
  #'
  #' @description Sometimes dates are represented as strings, potentially with
  #'   the month in the format "Dec" for example. This function handles a few of
  #'   these and allows them to be reformatted. It is conceptually similar to
  #'   the `as.Date` and `strptime` functions, but doesn't require the data to
  #'   actually be a date.
  #'
  #' @param x A vector containing strings in which the month is clear. For a
  #'   list, use `handled <- lapply(x, handle_dates)`.
  #' @param out How should the date be returned? Choose from any of the
  #'   following (using the example of 20th June 1991).
  #'   ```
  #'       "YYYY-MM-DD"     returns     "1991-06-20"
  #'       "YMD"            returns     "1991-06-20"
  #'       "DD-MM-YYYY"     returns     "20-06-1991"
  #'       "DMY"            returns     "20-06-1991"
  #'       "YYYY-Jan-DD"    returns     "1991-Jun-20"
  #'       "YBD"            returns     "1991-Jun-20"
  #'       "DD-Jan-YYYY"    returns     "20-Jun-1991"
  #'       "DBY"            returns     "20-Jun-1991"
  #'   ```
  #'   Input is case-insensitive. The divider can be set using the 'outSep'
  #'   argument.
  #' @param outSep Which symbol should be placed between the date and month?
  #'   Default is "-", but will accept any reasonable string.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Internal function, applied with lapply after prepping separators
  handle_dates_internal <- function(x, out, outSep) {
    # Guard 1: Date must have three parts
    x <- unlist(x)
    if (length(x) != 3) stop("Incorrect date format entered.")

    # Guard 2: One part must have four characters for the year
    xLengths   <- sapply(x, nchar)
    xYearIndex <- which(xLengths == 4)

    # Handle if the month also has 4 characters (e.g. June, July)
    if (2 %in% xYearIndex) {
      xYearIndex <- xYearIndex[-which(xYearIndex == 2)]
    }

    # If multiple date parts have 4, there's an error!
    if (length(xYearIndex) != 1) {
      stop("Incorrect date format entered. ",
           "Use YYYY-MM-DD or DD-MM-YYYY")
    }

    # Identify each part
    yearBit  <- x[xYearIndex]
    monthBit <- x[2] |> handle_months(out = "Jan") # this is an assumption
    dayBit   <- x[-c(2, xYearIndex)]

    # Format for returning
    if (out %in% c("yyyy-mm-dd", "ymd")) {
      y <- paste(sep = outSep,
                 yearBit,
                 handle_months(monthBit, out = "01"),
                 dayBit)
    } else if (out %in% c("dd-mm-yyyy", "dmy")) {
      y <- paste(sep = outSep,
                 dayBit,
                 handle_months(monthBit, out = "01"),
                 yearBit)
    } else if (out %in% c("dd-jan-yyyy", "dby")) {
      y <- paste(sep = outSep,
                 dayBit,
                 monthBit,
                 yearBit)
    } else if (out %in% c("yyyy-jan-dd", "ybd")) {
      y <- paste(sep = outSep,
                 yearBit,
                 monthBit,
                 dayBit)
    }

    return(y)
  }
  # Guard against NULL / NA / NaN
  if (is.null(x[1])) return(NULL)
  if (is.na(x[1]) | is.nan(x[1])) return(NA)

  # Handle if date contains " ", "/", or "-" separations
  x <- gsub(pattern = "/", replacement = "-", x = x) |>
    gsub(pattern = "_", replacement = "-") |>
    gsub(pattern = " ", replacement = "-")

  # Split and rearrange in correct output
  xSplit <- strsplit(x, "-")
  yDate  <- sapply(xSplit, handle_dates_internal,
                   out = tolower(out),
                   outSep = outSep)

  return(yDate)
}

