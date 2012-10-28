\name{RaeInv}
\alias{RaeInv}
\title{1 minus Rae index}
\description{Calculates 1 minus the Rae index.}
\usage{RaeInv(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as: 1 minus (the sum of the absolute differences |Z-R| divided by the number of categories).}
\value{Representation score}
\author{Didier Ruedin}
\seealso{\code{\link{Rae}}}
