\name{irs.plot}
\alias{irs.plot}
\title{Plot individual representation scores for categorical data.}
\description{Plots individual representation scores based on frequency vectors.}
\usage{
irs.plot(G, H, perspective = "individual", groups=c("Voters","MP"), concept="Ideology")
}
\arguments{
  \item{G}{frequency vector for the population}
  \item{H}{frequency vector for the representatives}
  \item{perspective}{individual perspective (default) or population perspective}
  \item{groups}{label for the frequency vectors}
  \item{concept}{label for the concept measures}
}
\details{This function plots individual representation scores for categorical data. As with the underlying \code{\link{irs}}, you will need two frequency vectors, one for the population, and one for the representatives. The two frequency vectors need to be of the same length. Use the argument \code{groups} to specify what these frequency capture. The default labels are Voters and MP. You can also specify the concept measured, such as left-right or social policies. It is further possible to choose the perspective of the population, which leads to slightly different individual representation scores for small N.

The plots show the frequency distributions of the two vectors supplied at the top, and the resulting individual representation scores for each category.}
\author{Didier Ruedin}
\references{Ruedin, D. (2012) Individual representation: A different approach to political representation. Representation 48(1): 115-29.}
\seealso{\code{\link{irs.cat}}}
\examples{
# Sample data
G <- c(1,5,10,15,3)
H <- c(0,3,0,5,1)
# Calculate individual representation scores
irs.cat(G,H)
# Plot this:
irs.plot(G,H)
}
