\name{relrepMax}
\alias{relrepMax}
\title{Calculates maximum relative representation scores of any minority group}
\description{Calculates relative representation scores for each group, based on a population vector and a vector for the representatives. Reports the largest relative representation score for any individual group, except for the first group in the vector.}
\usage{relrepMax(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{Relative representation scores (\code{\link{relrep}}) are calculated for each minority group. The majority group must be in first position of the vector. All groups in positions 2 to n are considered minority groups; no test is applied to check whether the groups are numerical minorities.}
\value{Representation score}
\author{Didier Ruedin}
\seealso{\code{\link{relrep}}, \code{\link{dHondt}}}
