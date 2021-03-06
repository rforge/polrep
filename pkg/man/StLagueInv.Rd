\name{StLagueInv}
\alias{StLagueInv}
\title{Inverted Sainte-Lague}
\description{Calculates inverted Sainte-Lague.}
\usage{StLagueInv(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as the inverted Sainte-Lague method: sum((R-Z)^2/Z).}
\value{Representation score}
\author{Didier Ruedin}
\seealso{\code{\link{StLague}}}
