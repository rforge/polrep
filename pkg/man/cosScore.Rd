\name{cosScore}
\alias{cosScore}
\title{Calculates Cosine Proportionality Scores}
\description{Calculates cosine proportionality scores based on a population vector and a vector for the representatives}
\usage{cosScore(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The order of the groups in he population vector and the vector for the representatives needs to be the same. If there are groups A, B, C in the population, both vectors need to cover their proportion in the same order.

The cosine proportionality scores were introduced by Koppel and Diskin (2009).
}
\value{A single score given the population and representatives}
\author{Didier Ruedin}
\references{Koppel, M., and A. Diskin. (2009) Measuring disproportionality, volatility and malapportionment: axiomatization and solutions. Social Choice and Welfare 33, no. 2: 281-286.}
