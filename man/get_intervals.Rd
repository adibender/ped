% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval-information.R
\name{get_intervals}
\alias{get_intervals}
\alias{get_intervals.default}
\title{Information on intervals in which times fall}
\usage{
get_intervals(x, times, ...)

\method{get_intervals}{default}(x, times, left.open = TRUE,
  rightmost.closed = TRUE, ...)
}
\arguments{
\item{x}{An object from which interval information can be obtained,
see \code{\link{int_info}}.}

\item{times}{A vector of times for which corresponding interval information
should be returned.}

\item{...}{Further arguments passed to \code{\link[base]{findInterval}}.}

\item{left.open}{logical; if true all the intervals are open at left
    and closed at right; in the formulas below, \eqn{\le} should be
    swapped with \eqn{<} (and \eqn{>} with \eqn{\ge}), and
    \code{rightmost.closed} means \sQuote{leftmost is closed}.  This may
    be useful, e.g., in survival analysis computations.}

\item{rightmost.closed}{logical; if true, the rightmost interval,
    \code{vec[N-1] .. vec[N]} is treated as \emph{closed}, see below.}
}
\value{
A \code{data.frame} containing information on intervals in which
values of \code{times} fall.
}
\description{
Information on intervals in which times fall
}
\examples{
set.seed(111018)
brks <- c(0, 4.5, 5, 10, 30)
int_info(brks)
x <- runif (3, 0, 30)
x
get_intervals(brks, x)

}
\seealso{
\code{\link[base]{findInterval}} \code{\link{int_info}}
}
