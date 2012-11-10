\name{scoreN}
\alias{scoreN}
\title{Representation score N}
\description{Calculates the (potential) representation score N.}
\usage{scoreN(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as the proportion of minorities in the population minus the proporiton of minorities among the representatives. All groups in the population except for the first one are considered minorities.}
\value{Representation score}
\author{Didier Ruedin}
