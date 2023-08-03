subset_by_summer <- function(x, summers,
                             australSplit = 3) {
  #' Subset a SpatRaster based on the layers' summer
  #'
  #' @description Easily select only layers of a SpatRaster that are in a
  #'   certain austral summer (e.g. Summer 1991 is 1990/1991). See the
  #'   australSplit argument for defining which months are considered in which
  #'   austral summer.
  #'
  #' @param x SpatRaster: The data to subset. Can be either a string, in which
  #'   case it is interpreted as a filePath and read in, or an existing
  #'   SpatRaster.
  #' @param summers vector: Which summer/s to return?
  #' @param australSplit numeric: Which is the last month included in an austral
  #'   summer before the new austral year begins? The default value is 3, which
  #'   means that all months *AFTER* March are considered as part of the
  #'   following summer (i.e. April 1991 -- March 1992 are all in 1992). Swap
  #'   this value according: setting it as 4 means May 1991 -- April 1992 are
  #'   all 1992.
  #'
  #' @export

  # Code -----------------------------------------------------------------------
  # Get dates of each layer
  xDates <- get_terra_dates(x, austral = australSplit)

  # Identify relevant layers
  summerIndex <- which(xDates$summer %in% summers)

  # Subset the data
  xSubset <- terra::subset(x, summerIndex)

  return(xSubset)
}
