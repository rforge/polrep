\name{score.U}
\alias{score.U}
\title{Representation score "U"}
\description{Calculates the (potential) representation score "U".}
\usage{score.U(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as Q-scores / heteregeneity of the population.

This measure is different from R-scores (\code{\link{R.score}}).}
\value{Representation score}
\author{Didier Ruedin}
\seealso{\code{\link{H}}, \code{\link{Q.score}}}
