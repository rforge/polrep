\name{amcc}
\alias{amcc}
\title{Absolute mean citizen congruence}
\description{Calculates absolute median citizen congruence.}
\usage{amcc(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives, or single point (e.g. government)}
}
\details{The representation score is calculated as: |median of Z - median of R|, or |median of Z - single point|. There is a derivative (\code{\link{amcc}}) which uses the mean rather than the median here.}
\value{Representation score}
\author{Didier Ruedin}
\references{Golder, M., and J. Stramski. 2010. Ideological Congruence and Electoral Institutions. American Journal of Political Science 54 (1): 90-106.}
\seealso{\code{\link{aacc}}}
