\name{aacc}
\alias{aacc}
\title{Absolute mean citizen congruence}
\description{Calculates absolute mean citizen congruence.}
\usage{aacc(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives, or single point (e.g. government)}
}
\details{The representation score is calculated as: |mean of Z - mean of R|, or |mean of Z - single point|. This is a derivative of \code{\link{amcc}} which uses the median rather than the mean here.}
\value{Representation score}
\author{Didier Ruedin}
\references{Golder, M., and J. Stramski. 2010. Ideological Congruence and Electoral Institutions. American Journal of Political Science 54 (1): 90-106.}
\seealso{\code{\link{amcc}}}
