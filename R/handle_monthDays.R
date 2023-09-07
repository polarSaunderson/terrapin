handle_monthDays <- function(x) {
  #' Reformat "month-day" information
  #'
  #' @description This function reformats month-day combinations to match
  #'   "Jun-04".
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
  #'   02/01). It also doesn't (currently) strip out any year information in
  #'   the date.
  #'
  #' @param x A vector containing strings in which the month is clear. For a
  #'   list, use `handled <- lapply(x, handle_monthDays)`.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Define function to call with lapply
  handle_monthDays_internally <- function(y) {
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
      as.numeric() |>
      sprintf(fmt = "%02d")

    # New date
    z <- paste(get_months(yMonth, out = "Jan"), yDay, sep = "-")
    return(z)
  }

  # Apply the function
  xOut <- sapply(x, handle_monthDays_internally)
}
