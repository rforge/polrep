\name{neffIncomp}
\alias{neffIncomp}
\title{Calculates effective number of parties for incomplete data}
\description{This function calculates the effective number of parties with incomplete data, as introduced by Taagepera (1997). It can be used to estimate the effective number of groups in a population.}
\usage{neffIncomp(x)}
\arguments{\item{x}{Population vector, the residual category (`other') needs to be last}}
\details{This function calculates the number of effective parties if data are incomplete, i.e. if there is a residual category (`other'). This function can be used to enumerate the effective number of parties, but also the effective number of groups in a population or representative body.

Note that the residual category is assumed to be the last item in the population vector. The best estimate (\code{Neff}) may be incorrect if certain small parties or groups are listed for historical (or other) importance. In this case, the error range would be underestimated.}
\value{Returns a list: \code{Neff} gives the best estimate (in the absence of other information), \code{simple} does not square the residual category, \code{low} treats the residual category a group on its own, \code{least} assumes the groups in the residual category to be at least as big as the smallest group given, and \code{high} excludes the residual category.}
\author{Didier Ruedin}
\references{Taagepera, R. (1997) Effective Number of Parties for Incoplete Data. Electoral Studies 16(2): 145-51.}
\seealso{\code{\link{neff}}}
