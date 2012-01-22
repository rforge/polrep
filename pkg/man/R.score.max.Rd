\name{R.score.max}
\alias{R.score.max}
\title{Calculates maximum R-score of any minority group}
\description{Calculates R-scores for each group, based on a population vector and a vector for the representatives. Reports the largest R-score for any individual group.}
\usage{R.score.max(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{R-scores (\code{\link{R.score}}) are calculated for each minority group. The majority group must be in first position of the vector. All groups in positions 2 to n are considered minority groups; no test is applied to check whether the groups are numerical minorities.}
\value{Representation score}
\author{Didier Ruedin}
