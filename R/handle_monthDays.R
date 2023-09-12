handle_monthDays <- function(x, out = "Jan-01", outSep = "-") {
  #' Reformat "month-day" information
  #'
  #' @description This function reformats month-day combinations to match
  #'   either "Jun-04", "04-Jun", "06-04", "04-06", "6-4", or "4-6". The
  #'   last are ambiguous but can be useful in defined situations, for example
  #'   pasting with a year to create a date.
  #'
  #'   ```
  #'       Jun-04
  #'       06-04
  #'       04-06
  #'       4-6
  #'       6-4
  #'   ````
  #'
  #'   This function should work as long as the month is unambiguous in a string
  #'   with a reasonable separator (see below). It can handle these inputs:
  #'   ```
  #'      01-Jan  | 1-Jan | Jan-1 | Jan-01
  #'      01/Jan  | 1/Jan | Jan/1 | Jan/01
  #'      01 Jan  | 1 Jan | Jan 1 | Jan 01
  #'      01_Jan  | 1_Jan | Jan_1 | Jan_01
  #'
  #'   The Jan can also be "jan", "January", or "jan".
  #'   ```
  #'   **note** This function cannot handle unambiguous combinations (e.g.
  #'   02/01) as input. It can return them (for e.g. pasting with a year).
  #'
  #'   It can also strip the month-date out of a date if it is entered in the
  #'   format YYYY-MM-DD or DD-MM-YYYY. But **NOT** MM-DD-YYYY.
  #'
  #' @param x A vector containing strings in which the month is clear. For a
  #'   list, use `handled <- lapply(x, handle_monthDays)`.
  #' @param out How should the monthDays be returned? Choose from any of the
  #'   following (using the examples of 20th June and 5th March). Note the
  #'   ambiguity in the last options.
  #'
  #'   ```
  #'       "Jan-01"   returns "Jun-20"   "Mar-05"
  #'       "01-Jan"   returns "20-Jun"   "05-Mar"
  #'       "mm-dd"    returns "06-20"    "03-05"
  #'       "dd-mm"    returns "20-06"    "05-03"
  #'       "m-d"      returns "6-20"     "3-5"
  #'       "d-m"      returns "20-6"     "5-3"
  #'   ```
  #'   The divider can be set using the 'outSep' argument.
  #'
  #' @param outSep Which symbol should be placed between the date and month?
  #'   Default is "-", but will accept any reasonable string.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Define internal function to call with lapply -------------------------------
  handle_monthDays_internally <- function(y, out, md, outSep) {
    # Find just the monthly part
    yMonth <- get_months(y, "asis")

    # Find where the monthly part is in the string
    greg <- gregexpr(pattern = yMonth, text = y) |> unlist()

    # Find length of the monthly part
    yLength <- nchar(yMonth)

    # Strip monthly part
    yDay <- gsub(pattern = yMonth, replacement = "", x = y) |>
      gsub(pattern = " ", replacement = "") |>
      gsub(pattern = "/", replacement = "") |>
      gsub(pattern = "-", replacement = "") |>
      gsub(pattern = "_", replacement = "") |>
      as.numeric()

    # Handle for the formatting
    if (out == "01" | out == 2) {
      yDay <- sprintf(fmt = "%02d", yDay)
      if (out == 2) {
        out <- "Jan"
      }
    } else if (out == 3) {
        out <- "Jan"
    }

    # Create the new date
    if (isTRUE(md)) {
      z <- paste(get_months(yMonth, out = out), yDay, sep = outSep)
    } else if (isFALSE(md)) {
      z <- paste(yDay, get_months(yMonth, out = out), sep = outSep)
    }
    return(z)
  }

  # Prepping for the internal function -----------------------------------------
  strip_years <- function(x) {
    # Year definition is based on 3 parts
    xLength <- strsplit(x, "-") |> lapply(length) |> unlist()
    xIsYear <- ifelse(test = (xLength == 3),
                      yes  = TRUE,
                      no   = FALSE)
    xWhich  <- which(xIsYear == 1)
    yYears  <- x[xWhich]

    # Year part is defined by a length of 4 characters in position 1 or 3
    ySplit  <- strsplit(yYears, "-")
    yToSwap <- sapply(X = ySplit,
                      FUN = function(ii){
                        # Which position have 4 characters?
                        iiLengths   <- lapply(ii, nchar) |> unlist()
                        iiYearIndex <- which(iiLengths == 4)

                        # Handle if the month had 4 characters (e.g. June, July)
                        if (2 %in% iiYearIndex) {
                          iiYearIndex <- iiYearIndex[-which(iiYearIndex == 2)]
                        }

                        # If multiple date parts have 4, there's an error!
                        if (length(iiYearIndex) != 1) {
                          stop("Incorrect date format entered. ",
                               "Use YYYY-MM-DD or DD-MM-YYYY")
                        }

                        # Make month unambiguous
                        ii[2] <- handle_months(ii[2], out = "Jan")

                        # Remove the year part
                        ii <- ii[-iiYearIndex]

                        # Collapse down
                        ii <- paste(ii, collapse = "-")

                        return(ii)
                      } )

    # Replace any entries in x that had the year in
    x[xWhich] <- yToSwap
    return(x)
  }

  # Prepare for different output options ---------------------------------------
  md  <- switch(tolower(out),
                "mm-dd" = , "m-d" = , "mmdd" = , "md" = , "jan-1" = , "jan-01" = TRUE,
                "dd-mm" = , "d-m" = , "ddmm" = , "dm" = , "1-jan" = , "01-jan" = FALSE,
                TRUE)                       # default is Jan-01

  out <- switch(tolower(out),
                "mm-dd"  = , "dd-mm"  = , "ddmm"  = , "mmdd" = "01",
                "m-d"    = , "d-m"    = , "dm"    = , "md"   = 1,
                "jan-01" = , "01-jan" = 2,
                "jan-1"  = , "1-jan"  = 3,  # will reset later on
                2)                          # default is Jan-01

  # Handle different date separators
  x <- gsub(pattern = "/", replacement = "-", x) |>
    gsub(pattern = "_", replacement = "-") |>
    gsub(pattern = " ", replacement = "-")

  # Remove any years in the dataset
  x <- strip_years(x)

  # Apply the function ---------------------------------------------------------
  xOut <- sapply(x, handle_monthDays_internally, out, md, outSep)

  return(xOut)

}
