\name{scoreR}
\alias{scoreR}
\title{Representation score R}
\description{Calculates the (potential) representation score R.}
\usage{scoreR(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as: sum(R/Z)*heterogeneity of the population.}
\value{Representation score}
\author{Didier Ruedin}
\seealso{\code{\link{H}}}
