\name{irs}
\alias{irs}
\title{Calculate individual representation scores.}
\description{Calculate individual representation scores based on a population vector and a vector for the representatives. For each individual, the vector needs to indicate the position.}
\usage{irs(P, R, perspective = "individual")}
\arguments{
  \item{P}{Population vector}
  \item{R}{Vector for the representatives}
  \item{perspective}{Option to use the "population" perspective}
}
\details{If you have categorical data (i.e. a limited number of positions), the function \code{\link{irs.cat}} is much faster.}
\value{A vector giving the individual representation score for each individual of the population.}
\author{Didier Ruedin}
\references{Ruedin, D. (2012) Individual representation: A different approach to political representation. Representation 48(1): 115-29.}
\seealso{\code{\link{irs.cat}}}
\examples{
# Sample data:
P <- c(1,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5)
R <- c(2,2,2,4,4,4,4,4,5)
# Calculate IRS:
irs(P,R)
# Gives IRS: a vector starting with: 0.04, 0.07, and ending with 0.08
}
