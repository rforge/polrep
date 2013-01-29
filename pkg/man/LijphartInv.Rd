\name{LijphartInv}
\alias{LijphartInv}
\title{Calculates an inverted Lijphart index of proportionality}
\description{Calculates an inverted Lijphart index of proportionality based on a population vector and a vector for the representatives}
\usage{LijphartInv(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The order of the groups in he population vector and the vector for the representatives needs to be the same. If there are groups A, B, C in the population, both vectors need to cover their proportion in the same order.

The inverted Lijphart index gives 1 minus the largest of the absolute differences between the proportion of the population and representatives in each group.
}
\value{A single score given the population and representatives}
\author{Didier Ruedin}
\references{Taagepera, R., and B. Grofman. Mapping the indices of seats-votes disproportionality and inter-election volatility. Party Politics 9, no. 6 (2003): 659-77.}
\seealso{\code{\link{Grofman}}, \code{\link{Rose}}}
