\name{score.G}
\alias{score.G}
\title{Representation score "G"}
\description{Calculates the (potential) representation score "G".}
\usage{score.G(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as: sum(abs(Z-R)/Z).}
\value{Representation score}
\author{Didier Ruedin}
