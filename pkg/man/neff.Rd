\name{neff}
\alias{neff}
\title{Calculates effective number of parties}
\description{This function calculates one version of the effective number of parties introduced by Laakso and Taagepera (1979). It can be used to estimate the effective number of groups in a population.}
\usage{neff(x)}
\arguments{  \item{x}{Population vector}}
\details{This is one of two functions to calculate the number of effective parties. It gives 1 divided by the sum of Z squared. This function can be used to enumerate the effective number of parties, but also the effective number of groups in a population or representative body.}
\value{Effective number of parties}
\author{Didier Ruedin}
\references{Laakso, M., and R. Taagepera (1979) The Effective Number of Parties: A Measure with Application to West Europe. Comparative Political Studies 12(1): 3-27.}
\seealso{\code{\link{neffProd}}, \code{\link{GTT}}, \code{\link{GTD2}}}
