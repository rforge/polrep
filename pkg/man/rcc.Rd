\name{rcc}
\alias{rcc}
\title{Relative citizen congruence}
\description{Calculates relative citizen congruence.}
\usage{rcc(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as: 1 - (sum |Zi - median(Z)) / (sum |Zi - median(R)).}
\value{Representation score}
\author{Didier Ruedin}
\references{Golder, M., and J. Stramski. 2010. Ideological Congruence and Electoral Institutions. American Journal of Political Science 54 (1): 90-106.}
\seealso{\code{\link{acc}}}
