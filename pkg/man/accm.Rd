\name{accm}
\alias{accm}
\title{Absolute citizen congruence}
\description{Calculates absolute citizen congruence using the mean.}
\usage{accm(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives, or single point (e.g. government)}
}
\details{The representation score is calculated as: (1/N) * sum|Zi - mean(R)|. This is a derivative of \code{\link{acc}} which uses the median rather than the mean here.
}
\value{Representation score}
\author{Didier Ruedin}
\references{Golder, M., and J. Stramski. 2010. Ideological Congruence and Electoral Institutions. American Journal of Political Science 54 (1): 90-106.}
\seealso{\code{\link{acc}}}
