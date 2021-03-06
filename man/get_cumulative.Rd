% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formula-specials.R
\name{get_cumulative}
\alias{get_cumulative}
\alias{expand_cumulative}
\title{Expand time-dependent covariates to functionals}
\usage{
get_cumulative(data, formula)

expand_cumulative(data, func, n_func)
}
\arguments{
\item{data}{Data frame (or similar) in which variables specified in ...
will be looked for}

\item{formula}{A formula containing \code{cumulative} specials,
that specify the type of cumulative effect one wants to estimate. For details
see the vignettes on data transformation and time-dependent covariates.}

\item{func}{Single evaluated \code{\link{cumulative}} term.}
}
\description{
Given formula specification on how time-dependent covariates affect the
outcome, creates respective functional covariate as well as auxiliary
matrices for time/latency etc.
}
\keyword{internal}
