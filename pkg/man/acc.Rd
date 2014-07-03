\name{acc}
\alias{acc}
\title{Absolute citizen congruence}
\description{Calculates absolute citizen congruence.}
\usage{acc(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives, or single point (e.g. government)}
}
\details{The representation score is calculated as: (1/N) * sum|Zi - median(R)|. There is a derivative (\code{\link{accm}}) which uses the mean rather than the median here.
}
\value{Representation score}
\author{Didier Ruedin}
\references{Golder, M., and J. Stramski. 2010. Ideological Congruence and Electoral Institutions. American Journal of Political Science 54 (1): 90-106.}
\seealso{\code{\link{accm}}, \code{\link{rcc}}}
