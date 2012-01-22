\name{Gallagher}
\alias{Gallagher}
\title{Gallagher index}
\description{Calculates the Gallagher index.}
\usage{Gallagher(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as: sqrt(sum((Z-R)^2)/2).}
\value{Representation score}
\author{Didier Ruedin}
\references{Gallagher, M. (1991) "Proportionality, disproportionality and electoral systems." Electoral Studies 10(1):33-51.}
