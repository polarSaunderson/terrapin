##
check_if_null <- function(...,
                           stopIfNoNull = TRUE,
                           noNullMessage = "No conditions selected!") {
  #' Checks only 1 option has been set
  #'
  #' @description If a function has multiple arguments that default to NULL,
  #'   this function checks that only one of them has been set at once.
  #'
  #' @param ... The arguments to check for NULL
  #' @param stopIfNoNull LOGICAL: If TRUE, an error is thrown; if FALSE, a
  #'   warning is thrown.
  #' @param noNullMessage "string" The warning or error message.
  #'
  #' @noRd

  # Code -----------------------------------------------------------------------
  dots <- list(...)            # all input
  nullCount <- length(dots)    # start counter

  # check if each input is null
  for (ii in seq_along(dots)) {
    iiBit <- dots[[ii]]
    if (is.null(iiBit)) nullCount <- nullCount - 1
  }

  # Act on the count
  if (nullCount == 0) {
    if (isTRUE(stopIfNoNull)) stop(noNullMessage)
    warning(noNullMessage)
  } else if (nullCount > 1) {
    stop("Only one condition can be used at once!")
  }
}

# Useful function here
# These are all taken from domR, v0.0.1.10

##
`%notIn%` <- function(x, y) {
  # Code -----------------------------------------------------------------------
  match(x, y, nomatch = 0) == 0
}

##
set_if_null <- function(x, defaultValue) {
  # Code -----------------------------------------------------------------------
    if (is.null(x)) x <- defaultValue
    return(x)
}
