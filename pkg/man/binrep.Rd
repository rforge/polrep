\name{binrep}
\alias{binrep}
\title{Determine binary representation}
\description{Determine if binary representation of minority groups exists. Although the function requires a vector for the population vector and a vector for the representatives, only the representatives' is evaluated.}
\usage{binrep(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{This function is not concerned with proportions, and a single representative from any of possibly multiple minority groups is considered as "minorities present". The majority group must be in first position of the vector. All groups in positions 2 to n are considered minority groups; no test is applied to check whether the groups are numerical minorities.}
\value{TRUE if minority groups are present among the representatives, FALSE if they are absent.}
\author{Didier Ruedin}
