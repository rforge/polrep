\name{GallagherMod}
\alias{GallagherMod}
\title{Modified Gallagher index}
\description{Calculates a modified Gallagher index.}
\usage{GallagherMod(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as: sqrt(sum((Z.mod-R.mod)^2)/2), where Z.mod is Z/sum(Zi^2)^.5, and R.mod is R/sum(Ri^2)^.5}
\value{Representation score}
\author{Didier Ruedin}
\references{Koppel, M. and Diskin, A. (2009) Measuring disproportionality, volatility and malapportionment: axiomatization and solutions. Social Choice and Welfare 33(2):281-6.}
\seealso{\code{\link{Gallagher}}}
