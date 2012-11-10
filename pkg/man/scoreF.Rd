\name{scoreF}
\alias{scoreF}
\title{Representation score F}
\description{Calculates the (potential) representation score F.}
\usage{scoreF(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as: sum((Z-R)^2/Z).}
\value{Representation score}
\author{Didier Ruedin}
