\name{sortVectors}
\alias{sortVectors}
\title{Sort two vectors}
\description{Sort two frequency vectors by the values in the first.}
\usage{sortVectors(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{This function takes two vectors of the same length, denoting for example the proportion of different groups in the population and among representatives. The function sorts both vectors by the first one. Put differently, the first vector will be sorted by decreasing value. The second vector will be sorted so that each position of the vector refers to the same group. This is a simple application of the \code{order} function in R.}
\value{A list with two vectors, Z and R, sorted by Z}
\author{Didier Ruedin}
