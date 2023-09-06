handle_months <- function(x, out = "01") {
  #' Reformat month data regardless of how it is entered
  #'
  #' @description Often it is necessary to swap between representations of
  #'   months but this is just pointless fiddly code. This function simply (via
  #'   the help of a horrific nested `ifelse`!) handles common inputs, and
  #'   returns them in a specified format.
  #'
  #'   Input and output can be in the following formats:
  #'
  #'       1          numeric
  #'       "1"        string
  #'       "01"       padded string
  #'       "Jan"      Capitalised abbreviation
  #'       "jan"      lower case abbreviation
  #'       "January"  Capitalised Name
  #'       "january"  lower case name
  #'       "J"        Capital initial - only as output
  #'       "j"        lower case initial - only as output
  #'
  #'   To re-emphasize, the initials cannot be used as input because they are
  #'   ambiguous. However, they can be returned if you really want to use them.
  #'
  #' @param x A vector of months in any of the above formats. If values don't
  #'   correspond to these formats, the value is silently returned as it was
  #'   input (no warning!).
  #' @param out How should the months be returned? Choose from any of the above
  #'   formats. If any other values are used, the numeric representation of the
  #'   month is used (e.g. 1 for January) or the input x, if it didn't
  #'   correspond to a valid month.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Account for different inputs
  x <- as.character(x)  # treat numbers as strings so we just deal with strings

  # Super fun nested ifelse
  y <- ifelse(test = tolower(x) %in% c("01", "1", "jan", "january"),
             yes  = 1,
             no   = ifelse(
               test = x %in% c("02", "2", "feb", "february"),
               yes  = 2,
               no   = ifelse(
                 test = x %in% c("03", "3", "mar", "march"),
                 yes  = 3,
                 no   = ifelse(
                   test = x %in% c("04", "1", "apr", "april"),
                   yes  = 4,
                   no   = ifelse(
                     test = x %in% c("05", "1", "may"),
                     yes  = 5,
                     no   = ifelse(
                       test = x %in% c("06", "6", "jun", "june"),
                       yes  = 6,
                       no   = ifelse(
                         test = x %in% c("07", "7", "jul", "july"),
                         yes  = 7,
                         no   = ifelse(
                           test = x %in% c("08", "8", "aug", "august"),
                           yes  = 8,
                           no   = ifelse(
                             test = x %in% c("09", "9", "sep", "september"),
                             yes  = 9,
                             no   = ifelse(
                               test = x %in% c("10", "oct", "october"),
                               yes  = 10,
                               no   = ifelse(
                                 test = x %in% c("11", "nov", "november"),
                                 yes  = 11,
                                 no   = ifelse(
                                   test = x %in% c("12", "dec", "december"),
                                   yes  = 12,
                                   no   = x)))))))))))) |> as.numeric()

  # Create output. Valid options: "01", "1", 1, Jan, jan, January, january
  if (is.numeric(out)) return(y)        # just give the numbers back

  # Else we want a string
  z <- switch(out,
              "1"       = as.character(y),
              "01"      = sprintf("%02d", as.numeric(y)),
              "January" = month.name[y],
              "january" = month.name[y] |> tolower(),
              "Jan"     = month.abb[y],
              "jan"     = month.abb[y] |> tolower(),
              "J"       = month.abb[y] |> substring(1, 1),
              "j"       = month.abb[y] |> towlower() |> substring(1, 1),
              y)        # just return the number string (or invalid months if entered)

  return(z)
}

#
#
# # THIS IS IN PROGRESS - I DON'T KNOW HOW TO IGNORE THIS BIT IF NOT A JJA
#
#
# # Account for if a vector of initials is entered (e.g. JJA)
# mString <- substring(month.abb, 1, 1) |> paste(collapse = "") |> strrep(2)
#
# # Define here for use in the lapply
# mini_matching_fun <- function(pattern, text) {
#   greg <- gregexpr(pattern, text)
#   index <- 0:(length(pattern) + 1) + unlist(greg)
#   index[index > 12] <- index[index > 12] - 12
#   return(index)
# }
# tt <- lapply(xxx, FUN = mini_matching_fun, text = mString)
#
