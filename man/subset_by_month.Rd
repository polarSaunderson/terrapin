% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset_by_month.R
\name{subset_by_month}
\alias{subset_by_month}
\title{Subset a SpatRaster based on the layers' month}
\usage{
subset_by_month(
  x,
  months = NULL,
  excludeIncomplete = FALSE,
  dailyResolution = FALSE,
  before = NULL,
  after = NULL,
  except = NULL
)
}
\arguments{
\item{x}{SpatRaster: The data to subset. It can be either a string, in
which case it is interpreted as a filepath and read in using
\code{\link[terra:rast]{terra::rast()}}; or an existing SpatRaster.}

\item{months}{Which month/s to return? Use this argument for exact matches
(e.g. c(1:4, 8). Cannot be used in conjunction with the 'before', 'after',
or 'except' arguments.

\strong{Note:} This argument is fed through \code{\link[=get_months]{get_months()}}, so this
argument can be input in different formats. For example, December can be
input as any of: 12, "12", "Dec", "dec", "December", or "december".}

\item{excludeIncomplete}{Be careful using this argument! It can
dramatically affect the output of this function. Please read the
Explanation in \code{\link[=exclude_incomplete_years]{exclude_incomplete_years()}} first.

If TRUE, the data is run through \code{exclude_incomplete_years()}, and
only data from the years containing all requested months are returned.

If numeric (between 1 and 12), the data is run through
\link{exclude_incomplete_summers}, and the value is used as the 'australSplit'
argument. This returns only data from the austral summers that contain
all requested months.

If any other value (including the default FALSE), the
\code{exclude_incomplete_x} functions are skipped, and all layers matching the
'months' argument are returned, regardless of the summer or year in which
they occur. See the examples.}

\item{dailyResolution}{BINARY: If using the 'excludeIncomplete' argument,
it is necessary to define whether the data is at a daily or monthly
resolution. See \code{\link[=exclude_incomplete_summers]{exclude_incomplete_summers()}} and
\code{\link[=exclude_incomplete_years]{exclude_incomplete_years()}} for more information.}

\item{before}{Only data that occurs before this value is returned. Cannot
be used in conjunction with the exact values, 'after' or 'except'
arguments.}

\item{after}{Only data that occurs after this value is returned. Cannot be
used in conjunction with the exact values, 'before' or 'except'
arguments.}

\item{except}{vector: Which data should be excluded from the SpatRaster?
Cannot be used in conjunction with the exact values, 'after' or 'before'
arguments.}
}
\description{
Easily select only the layers of a SpatRaster depending on
only the layers' month. For example, only data in June, or only data
after October.
}
