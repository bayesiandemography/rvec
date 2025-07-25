% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distributions.R
\name{dpois_rvec}
\alias{dpois_rvec}
\alias{ppois_rvec}
\alias{qpois_rvec}
\alias{rpois_rvec}
\title{The Poisson Distribution, Using Multiple Draws}
\usage{
dpois_rvec(x, lambda, log = FALSE)

ppois_rvec(q, lambda, lower.tail = TRUE, log.p = FALSE)

qpois_rvec(p, lambda, lower.tail = TRUE, log.p = FALSE)

rpois_rvec(n, lambda, n_draw = NULL)
}
\arguments{
\item{x}{Quantiles. Can be an rvec.}

\item{lambda}{Vector of means.
See \code{\link[stats:Poisson]{stats::rpois()}}. Can be an rvec.}

\item{log, log.p}{Whether to return results
on a log scale. Default is
\code{FALSE}. Cannot be an rvec.}

\item{q}{Quantiles. Can be an rvec.}

\item{lower.tail}{Whether to return
\eqn{P[X \le x]}, as opposed to
\eqn{P[X > x]}. Default is \code{TRUE}.
Cannot be an rvec.}

\item{p}{Probabilities. Can be an rvec.}

\item{n}{The length of random vector being
created. Cannot be an rvec.}

\item{n_draw}{Number of random draws
in the random vector being
created. Cannot be an rvec.}
}
\value{
\itemize{
\item If any of the arguments are rvecs,
or if a value for \code{n_draw} is supplied,
then an \link[=rvec]{rvec}
\item Otherwise an ordinary R vector.
\item \code{rpois_rvec()} always returns doubles (not integers).
}
}
\description{
Density, distribution function,
quantile function and random generation for the
Poisson distribution, modified to work with
rvecs.
}
\details{
Functions \code{dpois_rvec()}, \code{ppois_rvec()},
\code{ppois_rvec()} and \code{rpois_rvec()} work like
base R functions \code{\link[=dpois]{dpois()}}, \code{\link[=ppois]{ppois()}},
\code{\link[=qpois]{qpois()}}, and \code{\link[=rpois]{rpois()}}, except that
they accept rvecs as inputs. If any
input is an rvec, then the output will be too.
Function \code{rpois_rvec()} also returns an
rvec if a value for \code{n_draw} is supplied.

\code{dpois_rvec()}, \code{ppois_rvec()},
\code{ppois_rvec()} and \code{rpois_rvec()}
use \link[vctrs:theory-faq-recycling]{tidyverse}
vector recycling rules:
\itemize{
\item Vectors of length 1 are recycled
\item All other vectors must have the same size
}
}
\section{Warning}{


From version rvec version 0.7.4 onwards,
\code{rpois_rvec()} always returns doubles (not integers).
}

\examples{
x <- rvec(list(c(3, 5),
               c(1, 2)))
dpois_rvec(x, lambda = 3)
ppois_rvec(x, lambda = 3)

rpois_rvec(n = 2,
           lambda = c(5, 10),
           n_draw = 1000)
}
\seealso{
\itemize{
\item \code{\link[=dpois]{dpois()}}
\item \code{\link[=ppois]{ppois()}}
\item \code{\link[=qpois]{qpois()}}
\item \code{\link[=rpois]{rpois()}}
\item \link[stats:Distributions]{stats::distributions}.
}
}
