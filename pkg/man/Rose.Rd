\name{Rose}
\alias{Rose}
\title{Calculates the Rose Index of Proportionality}
\description{Calculates the Rose Index of proportionality based on a population vector and a vector for the representatives}
\usage{Rose(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The order of the groups in he population vector and the vector for the representatives needs to be the same. If there are groups A, B, C in the population, both vectors need to cover their proportion in the same order.

The Rose index is the sum the absolute differences between the proportion of the population and representatives in each group. A division by 2 is used to standardize values between 0 and 1, and a subtraction from 1 is used so that higher values correspond to smaller differences. The Rose Index is also known as RIP (Rose index of proportionality), or 1 minus LHI (Loosemore-Hanby Index).
}
\value{A single score given the population and representatives}
\author{Didier Ruedin}
\seealso{\code{\link{LHI}}, \code{\link{Gallagher}}}
