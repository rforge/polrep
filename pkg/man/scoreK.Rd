\name{scoreK}
\alias{scoreK}
\title{Representation score K}
\description{Calculates the (potential) representation score K.}
\usage{scoreK(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as: (1/number of groups) * sum((R/Z)^2).}
\value{Representation score}
\author{Didier Ruedin}
