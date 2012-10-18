\name{Gallagher.1}
\alias{Gallagher.1}
\title{1 minus Gallagher index}
\description{Calculates 1 minus the Gallagher index.}
\usage{Gallagher.1(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as: 1 - (sqrt(sum((Z-R)^2)/2)).}
\value{Representation score}
\author{Didier Ruedin}
\seealso{\code{\link{Gallagher}}}
