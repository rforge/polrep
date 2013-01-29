\name{dHondt}
\alias{dHondt}
\title{Calculates maximum relative representation scores of any group}
\description{Calculates R-scores for each group, based on a population vector and a vector for the representatives. Reports the largest R-score for any individual group.}
\usage{dHondt(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{Relative representation scores (\code{\link{relrep}}) are calculated for each minority group. In contrast to \code{\link{relrep}}, no distinction between minority and majority groups is made.}
\value{Representation score}
\author{Didier Ruedin}
\seealso{\code{\link{relrep}}}
