% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_draw.R
\name{n_draw}
\alias{n_draw}
\alias{n_draw.rvec}
\title{Number of draws in a random sample}
\usage{
n_draw(x)

\method{n_draw}{rvec}(x)
}
\arguments{
\item{x}{An object holding a random sample.}
}
\value{
A count, or \code{NULL}, if the
vector being sampled as no elements.
}
\description{
Number of draws in a random sample
}
\examples{
m <- matrix(1:40, nrow = 4, ncol = 10)
x <- rvec(m)
n_draw(x)
}
