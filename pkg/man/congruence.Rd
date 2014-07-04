\name{congruence}
\alias{congruence}
\title{Congruence}
\description{Calculates congruence.}
\usage{congruence(Z, R, from=NA, to=NA, steps=10000)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
  \item{from}{Beginning of conceptual space}
  \item{to}{End of conceptual space}
  \item{steps}{Number of steps at which to evaluate the CDF}
}
\details{The representation score is calculated as the area between the cumulative distribution functions for the population and representatives: sum |cdf(Z) - cdf(R)|/n, where n is the distance between the minimum and maximum (to standardize). The beginning and end of the conceptual (policy) space can be specified if the vectors do not include the entire range of possible values. The number of steps at which the CDF are evaluated can be specified (default = 10000).}
\value{Representation score}
\author{Didier Ruedin}
\references{Golder, M., and J. Stramski. 2010. Ideological Congruence and Electoral Institutions. American Journal of Political Science 54 (1): 90-106.}
\seealso{\code{\link{rcc}}}
