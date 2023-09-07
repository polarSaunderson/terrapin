get_months <- function(x, out = "Jan", throwError = TRUE) {
  #' Find the month within common date formats
  #'
  #' @description Sometimes it makes sense to use a full date ("15-January-2001"),
  #'   but often it is much more common to use (e.g.) "Jan-15", "January 15", or
  #'   even just in reverse - "2001-January-15". This function simply extracts
  #'   the January part of the date, and returns it in the specified format.
  #'   See "Accepted Formats" below; this is implemented with [handle_months()].
  #'   It is also possible to use "asis", which returns the month part as it was
  #'   input (but ignores any day or year parts).
  #'
  #'   **Note:** Dates can be separated with any of these: " ", "_", "-", "/".
  #'
  #'   **Note:** The function will not accept dates such as "01-02", because it
  #'   is unclear whether that refers to January 2nd or 1st February. However,
  #'   it understands that "01-13" or "13-01" both refer to January (i.e. there
  #'   is no month 13!).
  #'
  #'   **Note:** It **will not** accept the MM-DD-YYYY format. It **will**
  #'   accept either DD-MM-YYYY, D-M-YY, or YYYY-MM-DD, etc. The function
  #'   essentially checks if the input has three parts, and returns the middle
  #'   value if so.
  #'
  #' @inheritSection handle_months Accepted Formats
  #'
  #' @param x A vector of dates in any of the accepted formats.
  #' @inheritParams handle_months
  #' @param throwError If the input format isn't valid, should an error be
  #'   thrown? TRUE by default. If FALSE, a warning is shown, and the input
  #'   value is returned as it was. If NULL, no warnings are shown, and the
  #'   value returned as it was.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Define function used w/ lapply to check input elements & extract the month
  identify_month <- function(x) {
    # Test 1 - Is the date already in there? -----------------------------------
    y <- tolower(x)
    monthRef <- tolower(c(month.abb, month.name))
    z1 <- x[y %in% monthRef]
    if (length(z1) > 0) return(z1)

    # Test 2 - Is it just a single number (e.g. "1" or "01")? ------------------
    z2 <- strsplit(x, "-") |> unlist()
    if (length(y) == 1) return(z2)

    # Test 3 - Is the format YYYY-MM-DD or DD-MM-YYYY? -------------------------
    # This WILL BE INCORRECT if MM-DD-YYYY because that makes no sense. We must
    # assume that the middle values are the month.
    y  <- strsplit(x, "-")
    if (length(y) == 3) {
      z3 <- y[[2]]
      if (length(z3) > 0) {
        if (as.numeric(z3) > 12) {
          stop("Cannot be in the format MM-DD-YYYY.")
        } else {
          return(z3)
        }
      }
    }

    # Test 4 - If only 2 numbers, is one clearly a date (i.e. > 12)? -----------
    if (length(y) == 2) {
      y1 <- y[[1]]
      y2 <- y[[2]]
      if (y1 > 12 & y2 > 12) {
        msgBit <- "Incorrect date format entered. "
        if (isTRUE(throwError)) stop(msgBit)
        if (is.null(throwError)) return(x)
        warning(msgBit, "Returning input.")
        return(x)
      }

      if (y1 > 12) {
        return(y2)
      } else if (y2 > 12) {
        return(y1)
      } else {
        msgBit <- "Ambiguous date format entered. "
        if (isTRUE(throwError)) stop(msgBit)
        if (is.null(throwError)) return(x)
        warning(msgBit, "Returning input.")
        return(x)
      }
    }

    # Anything else ------------------------------------------------------------
    msgBit <- "Unknown date format. "
    if (isTRUE(throwError)) stop(msgBit)
    if (is.null(throwError)) return(x)
    warning(msgBit, "Returning input.")
    return(x)
  }

  # Applying the function to get the month out! --------------------------------
  # Handle if date contains " ", "/", or "-" separations
  x <- gsub(pattern = "/", replacement = "-", x = x) |>
    gsub(pattern = "_", replacement = "-") |>
    gsub(pattern = " ", replacement = "-")

  # Get the month
  xSplit <- strsplit(x, "-")
  xMonth <- lapply(xSplit, identify_month)

  # Format
  if (out != "asis") {
    xOut <- terrapin::handle_months(xMonth, out = out)
  } else {
    xOut <- unlist(xMonth)
  }

  # Return
  return(xOut)
}
