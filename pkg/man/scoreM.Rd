\name{scoreM}
\alias{scoreM}
\title{Representation score M}
\description{Calculates the (potential) representation score M.}
\usage{scoreM(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as the proportion of minorities among the representatives; the population vector is discarded. All groups in the population except for the first one are considered minorities.}
\value{Representation score}
\author{Didier Ruedin}
