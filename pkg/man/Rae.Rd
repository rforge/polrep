\name{Rae}
\alias{Rae}
\title{Rae index}
\description{Calculates the Rae index.}
\usage{Rae(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as: the sum of the absolute differences |Z-R| divided by the number of categories. The Rae index is also known as RID (Rae index of disproportionality).}
\value{Representation score}
\author{Didier Ruedin}
\references{Rae, D. (1967) The Political Consequences of Electoral Laws. London: Yale University Press.}
\seealso{\code{\link{RaeInv}}}
