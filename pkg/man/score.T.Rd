\name{score.T}
\alias{score.T}
\title{Representation score "T"}
\description{Calculates the (potential) representation score "T".}
\usage{score.T(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as Q-scores * heteregeneity of the population.

This measure is different from R-scores (\code{\link{R.score}}).}
\value{Representation score}
\author{Didier Ruedin}
\seealso{\code{\link{H}}, \code{\link{Q.score}}}
