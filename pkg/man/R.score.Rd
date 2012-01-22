\name{R.score}
\alias{R.score}
\title{Calculates R-scores}
\description{Calculates R-scores based on a population vector and a vector for the representatives.}
\usage{R.score(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{R-scores combine all minority groups, and calculate the proportion of the combined minorities in the representatives by the proportion of the combined minorities in the population. The majority group must be in first position of the vector. All groups in positions 2 to n are considered minority groups; no test is applied to check whether the groups are numerical minorities.}
\value{Representation score}
\author{Didier Ruedin}
