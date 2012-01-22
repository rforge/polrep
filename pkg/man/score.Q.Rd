\name{score.Q}
\alias{score.Q}
\title{Representation score "Q"}
\description{Calculates the (potential) representation score "Q".}
\usage{score.Q(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as: 1/(sum(Z^2/R)).

This measure is different from Q-scores (\code{\link{Q.score}}).}
\value{Representation score}
\author{Didier Ruedin}
