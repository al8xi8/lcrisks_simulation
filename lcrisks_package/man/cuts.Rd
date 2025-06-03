\name{cuts}
\alias{cuts}
\title{Wrapper to the cut() function}
\usage{
cuts(data, npieces, simple.labels = TRUE, ...)
}
\arguments{
\item{data}{A numeric vector}
\item{npieces}{Number of cut points or numeric vector of cut points}
\item{simple.labels}{TRUE or FALSE}
\item{...}{Other arguments passed into \code{\link[base]{cut}}}
}

\description{
A wrapper to the \code{\link[base]{cut}} function, so that you can automatically break into quantiles as
the default behavior, otherwise if the breakpoints are included, then just break on those.
In all cases, include.lowest is set to True
}
