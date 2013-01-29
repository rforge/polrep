\name{CoxShugartInv}
\alias{CoxShugartInv}
\title{Inverted Cox-Shugart measure of proportionality}
\description{Calculate the Cox-Shugart measure of proportionality}
\usage{CoxShugartInv(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{This function calculates the Cox-Shugart measure of proportionality. It provides the slope of the regression line. For perfect representation, a score of 1 is calculated, for values smaller than 1, smaller groups among the representatives are numerically over-represented. This is the inverse regression of \code{\link{CoxShugart}}.}
\value{A single score given the population and representatives}
\author{Didier Ruedin}
\seealso{\code{\link{CoxShugart}}}
