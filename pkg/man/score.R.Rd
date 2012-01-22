\name{score.R}
\alias{score.R}
\title{Representation score "R"}
\description{Calculates the (potential) representation score "R".}
\usage{score.R(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as: sum(R/Z)*heterogeneity of the population.

This measure is different from R-scores (\code{\link{R.score}}).}
\value{Representation score}
\author{Didier Ruedin}
\seealso{\code{\link{H}}}
