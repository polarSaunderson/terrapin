correlate_by_cell <- function(xy,
                              detrend,
                              method = "pearson") {
  #' Calculates the per-pixel correlation between 2 SpatRasters
  #'
  #' @description Say we have a temperature SpatRaster with 20 layers, and a
  #'   precipitation SpatRaster with 20 layers. We want to create a map that
  #'   shows the correlation between temperature and precipitation across the 20
  #'   years for *each* pixel. Use this function and [terra::app()]. This
  #'   function largely follows the code from Robert Hijmans here:
  #'   [https://stackoverflow.com/questions/16698048/r-calculating-pearson-correlation-coefficient-in-each-cell-along-time-line]().
  #'
  #'   It is necessary to first combine the two datasets, using `c()`, and then
  #'   run the [terra::app()] function. See examples.
  #'
  #'   A SpatRaster with 2 layers is returned; the first is the correlation
  #'   estimate and the second is the p-value.
  #'
  #' @param xy SpatRaster: The dataset, which should be `c(x, y)`.
  #' @param detrend BINARY: Should the time series be detrended before the
  #'   correlation? Detrending is performed by using the residuals of the a
  #'   linear fit: `stats::resid(stats::lm(xx ~ seq_along(xx))`.
  #' @param method "string": Which correlation coefficient to use? One of
  #'   "pearson", "kendall", or "spearman". See [stats::cor.test()].
  #'
  #' @examples
  #'   \dontrun{
  #'     # We want to correlate x with y
  #'     x <- terra::rast("precip.nc")   # n layers
  #'     y <- terra::rast("temp.nc")     # also n layers
  #'
  #'     # Combine
  #'     xy <- c(x, y)
  #'
  #'     # Correlate using terra::app
  #'     koral <- terra::app(x = xy,
  #'                         detrend = TRUE,
  #'                         method  = "pearson",
  #'                         fun = correlate_by_cell)
  #'
  #'     names(koral) <- c("estimate", "pValue")
  #'   }
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
      xx <- stats::resid(stats::lm(xx ~ seq_along(xx),
                                   na.action = stats::na.exclude))
      yy <- stats::resid(stats::lm(yy ~ seq_along(yy),
                                   na.action = stats::na.exclude))
    }

    # Correlate & return
    kaw <- stats::cor.test(xx, yy, method = method) |>
      suppressWarnings()                  # if standard deviation is 0
    return(c(kaw$estimate, kaw$p.value))
  }
}
