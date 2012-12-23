\name{LHI}
\alias{LHI}
\title{Calculates the Rose Index of Proportionality}
\description{Calculates the Loosemore-Hanby Index (LHI) of distortion based on a population vector and a vector for the representatives}
\usage{LHI(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The order of the groups in he population vector and the vector for the representatives needs to be the same. If there are groups A, B, C in the population, both vectors need to cover their proportion in the same order.

The LHI is the sum the absolute differences between the proportion of the population and representatives in each group. A division by 2 is used to standardize values between 0 and 1. The LHI (1971) is also known as index of disproportionality, introduced earlier by Duncan and Duncan (1955), or as Pedersen index in volatility studies.
}
\value{A single score given the population and representatives}
\author{Didier Ruedin}
\references{Duncan, O. and Duncan, B. (1955) A methodological analysis of segregation indexes. American Sociological Review 20:210-7.

Loosemore, J. and Hanby, V. (1971) The theoretical limits of maximum distortion: Som analytical expressions for electoral systems. British Journal of Political Science 1:467-77}
\seealso{\code{\link{Rose}}}
