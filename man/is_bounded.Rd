% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assertions.R
\name{is_bounded}
\alias{is_bounded}
\title{Check whether a value within a certain interval}
\usage{
is_bounded(
  x,
  lower_bound = -Inf,
  lower_closed = FALSE,
  upper_bound = Inf,
  upper_closed = FALSE
)
}
\arguments{
\item{x}{A value to be tested}

\item{lower_bound}{The lower bound}

\item{lower_closed}{Whether the lower bound is closed}

\item{upper_bound}{The upper bound}

\item{upper_closed}{Whether the upper bound is closed}
}
\description{
Check whether a value within a certain interval
}
\examples{
\dontrun{
library(assertthat)
assert_that(is_bounded(10))
# TRUE

assert_that(is_bounded(10:30))
# TRUE

assert_that(is_bounded(Inf))
# Error: Inf is not bounded by (-Inf,Inf)

assert_that(is_bounded(10, lower_bound = 20))
# Error: 10 is not bounded by (20,Inf)

assert_that(is_bounded(
  10,
  lower_bound = 20,
  lower_closed = TRUE,
  upper_bound = 30,
  upper_closed = FALSE
))
# Error: 10 is not bounded by [20,30)
}
}
