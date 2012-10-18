\name{score.S}
\alias{score.S}
\title{Representation score S}
\description{Calculates the (potential) representation score S.}
\usage{score.S(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as: sum(R/Z)/(heterogeneity of population).}
\value{Representation score}
\author{Didier Ruedin}
\seealso{\code{\link{H}}}
