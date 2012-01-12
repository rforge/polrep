\name{irs.cat}
\alias{irs.cat}
\title{Calculate individual representation scores for categorical data.}
\description{This function calculates individual representation scores based on frequency vectors. If the data are in this form, this function is much faster than the standard IRS function.}
\usage{
irs.cat(G, H, perspective = "individual")
}
\arguments{
  \item{G}{frequency vector for the population}
  \item{H}{frequency vector for the representatives}
  \item{perspective}{individual perspective (default) or population perspective}
}
\details{This function calculates individual representation scores. You will need two frequency vectors, one for the population, and one for the representatives. The two frequency vectors need to be of the same length. It is possible to choose the perspective of the population, which leads to slightly different individual representation scores for small N.}
\value{A vector of the same length as the frequency vector for the population. For each position, the individual representation scores are given.}
\author{Didier Ruedin}
\seealso{\code{\link{irs}}}
\examples{
# Sample data
G <- c(1,5,10,15,3)
H <- c(0,3,0,5,1)
# Calculate individual representation scores
irs.cat(G,H)
# The representation scores for each position are:
# 5.65  2.05 -1.70 -2.95  3.75
# We have small N, so let's look at the population perspective
irs.cat(G, H, perspective = "population")
# The representation scores for each position are now:
# 4.2  1.0 -3.0 -5.0  2.6
}
