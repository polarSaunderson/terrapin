correlate_SpatRasters_by_cell <- function(xy, detrend, method) {
  #' Calculates the per-pixel correlation between 2 SpatRasters
  #'
  #' @description Say we have a temperature SpatRaster with 20 layers, and a
  #'   precipitation SpatRaster with 20 layers. We want to create a map that
  #'   shows the correlation between temperature and precipitation across the 20
  #'   years for *each* pixel. Use this function and `terra::app()`. This
  #'   function largely follows the code from Robert Hijmans here:
  #'   https://stackoverflow.com/questions/16698048/r-calculating-pearson-correlation-coefficient-in-each-cell-along-time-line
  #'
  #'   It is necessary to first combine the two datasets, using `c()`, and then
  #'   run the `terra::app()` function. See examples.
  #'
  #' @param xy SpatRaster: The dataset, which should be `c(x, y)`.
  #' @param detrend BINARY: Should the time series be detrended before the
  #'   correlation?
  #' @param method "string": Which correlation coefficient to use? One of
  #'   "pearson", "kendall", or "spearman". See `stats::cor.test()`.
  #'
  #' @export

  # Code ----------------------------------------------------------------------!
  xy <- stats::na.omit(matrix(xy, ncol = 2))  # split raster into 2 columns
  if (nrow(xy) == 0) {                 # Skip correlation if pixel only has NA
    return(c(NA, NA))
  } else {
    xx <- xy[, 1]            # 1st raster data
    yy <- xy[, 2]            # 2nd raster data

    # Detrend using a linear model
    if (isTRUE(detrend)) {
      # xx <- domR::linear_detrend(xx)      # require domR
      # yy <- domR::linear_detrend(yy)
      xx <- stats::resid(stats::lm(xx ~ seq_along(xx), na.action = stats::na.exclude))
      yy <- stats::resid(stats::lm(yy ~ seq_along(yy), na.action = stats::na.exclude))
    }

    # Correlate & return
    kaw <- stats::cor.test(xx, yy, method) |>
      suppressWarnings()                  # if standard deviation is 0
    return(c(kaw$estimate, kaw$p.value))
  }
}
