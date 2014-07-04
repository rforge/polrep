\name{congruencePlot}
\alias{congruencePlot}
\title{Congruence Plot}
\description{Plots congruence.}
\usage{congruencePlot(Z, R, from=NA, to=NA, steps=10000, Zcol=1, Rcol=1,
Zlty=1, Rlty=2, Pcol=220, Pdensity=NULL, Pangle=45, ...)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
  \item{from}{Beginning of conceptual space}
  \item{to}{End of conceptual space}
  \item{steps}{Number of steps at which to evaluate the CDF}
  \item{Zcol}{Line colour for the population vector}
  \item{Rcol}{Line colour for the vector of the representatives}
  \item{Zlty}{Line type for the population vector}
  \item{Rlty}{Line type for the vector of the representatives}
  \item{Pcol}{Colour of the polygon}
  \item{Pdensity}{Density of the polygon's pattern}
  \item{Pangle}{Angle of the polygon's pattern}
  \item{...}{Additional arguments for the plot}
}
\details{This function plots the cumulative distributions of the population vector an the vector for the representatives. A polygon highlights the area between the two curves. The basic arguments are the same as for the \code{\link{congruence}} function which calculates the area between the two curves; the additional arguments are used to control the lines (colour, type) and polygon (colour, pattern). Use \code{Pcol=NULL} to hide the polygon entirely. The beginning and end of the conceptual (policy) space can be specified if the vectors do not include the entire range of possible values. The number of steps at which the CDF are evaluated can be specified (default = 10000). Additional arguments can be passed to control the plot, like main title or axes.}
\author{Didier Ruedin}
\seealso{\code{\link{congruence}}}
