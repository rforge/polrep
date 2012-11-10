\name{diffScore}
\alias{diffScore}
\title{Difference between proportion of minorities in populaiton and representatives}
\description{Calculates the difference between the proportion of minorities in populaiton and the proportion of minorities among the representatives.}
\usage{diffScore(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The majority group must be in first position of the vector. All groups in positions 2 to n are considered minority groups; no test is applied to check whether the groups are numerical minorities.}
\value{Difference in proportions}
\author{Didier Ruedin}
