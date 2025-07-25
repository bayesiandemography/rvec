% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distributions.R
\name{dhyper_rvec}
\alias{dhyper_rvec}
\alias{phyper_rvec}
\alias{qhyper_rvec}
\alias{rhyper_rvec}
\title{The Hypergeometric Distribution, Using Multiple Draws}
\usage{
dhyper_rvec(x, m, n, k, log = FALSE)

phyper_rvec(q, m, n, k, lower.tail = TRUE, log.p = FALSE)

qhyper_rvec(p, m, n, k, lower.tail = TRUE, log.p = FALSE)

rhyper_rvec(nn, m, n, k, n_draw = NULL)
}
\arguments{
\item{x}{Quantiles. Can be an rvec.}

\item{m}{Number of white balls in the urn.
See \code{\link[stats:Hypergeometric]{stats::dhyper()}}. Can be an rvec.}

\item{n}{Number of black balls
in the urn. See \code{\link[stats:Hypergeometric]{stats::rhyper()}}.
Can be an rvec.}

\item{k}{Number of balls drawn from urn.
See \code{\link[stats:Hypergeometric]{stats::dhyper()}}. Can be an rvec.}

\item{log, log.p}{Whether to return results
on a log scale. Default is
\code{FALSE}. Cannot be an rvec.}

\item{q}{Quantiles. Can be an rvec.}

\item{lower.tail}{Whether to return
\eqn{P[X \le x]}, as opposed to
\eqn{P[X > x]}. Default is \code{TRUE}.
Cannot be an rvec.}

\item{p}{Probabilities. Can be an rvec.}

\item{nn}{The length of the random vector
being created. The equivalent of \code{n} in
other random variate functions.
See \code{\link[stats:Hypergeometric]{stats::rhyper()}}. Cannot be an rvec.}

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
\item \code{rhyper_rvec()} always returns doubles (not integers).
}
}
\description{
Density, distribution function,
quantile function and random generation for the
hypergeometric distribution, modified to work with
rvecs.
}
\details{
Functions \code{dhyper_rvec()}, \code{phyper_rvec()},
\code{phyper_rvec()} and \code{rhyper_rvec()} work like
base R functions \code{\link[=dhyper]{dhyper()}}, \code{\link[=phyper]{phyper()}},
\code{\link[=qhyper]{qhyper()}}, and \code{\link[=rhyper]{rhyper()}}, except that
they accept rvecs as inputs. If any
input is an rvec, then the output will be too.
Function \code{rhyper_rvec()} also returns an
rvec if a value for \code{n_draw} is supplied.

\code{dhyper_rvec()}, \code{phyper_rvec()},
\code{phyper_rvec()} and \code{rhyper_rvec()}
use \link[vctrs:theory-faq-recycling]{tidyverse}
vector recycling rules:
\itemize{
\item Vectors of length 1 are recycled
\item All other vectors must have the same size
}
}
\section{Warning}{


From version rvec version 0.7.4 onwards,
\code{rhyper_rvec()} always returns doubles (not integers).
}

\examples{
x <- rvec(list(c(3, 5),
               c(0, 2)))
dhyper_rvec(x, m = 6, n = 6, k = 5)
phyper_rvec(x, m = 6, n = 6, k = 5)

rhyper_rvec(nn = 2,
            k = c(3, 5),
            m = 6,
            n = 6,
            n_draw = 1000)
}
\seealso{
\itemize{
\item \code{\link[=dhyper]{dhyper()}}
\item \code{\link[=phyper]{phyper()}}
\item \code{\link[=qhyper]{qhyper()}}
\item \code{\link[=rhyper]{rhyper()}}
\item \link[stats:Distributions]{stats::distributions}.
}
}
