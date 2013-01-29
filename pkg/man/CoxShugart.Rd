\name{CoxShugart}
\alias{CoxShugart}
\title{Cox-Shugart measure of proportionality}
\description{Calculate the Cox-Shugart measure of proportionality}
\usage{CoxShugart(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{This function calculates the Cox-Shugart measure of proportionality. It provides the slope of the regression line. For perfect representation, a score of 1 is calculated, for values smaller than 1, smaller groups in population are numerically over-represented. The Cox-Shugart measure of proportionality indicates the direction of mis-representation. It is also known as the regression index.}
\value{A single score given the population and representatives}
\author{Didier Ruedin}
\seealso{\code{\link{CoxShugartInv}}}
