\name{frequencyvector}
\alias{frequencyvector}
\title{Convert a population vector into a frequency vector}
\description{This function converts a population vector into a frequency vector. The alias \code{\link{collapse}} can also be used.}
\usage{frequencyvector(D,pos=FALSE)}
\arguments{
  \item{D}{Population vector}
  \item{pos}{Optional: position of categories}
}
\details{This function converts a population vector into a frequency vector. A population vector has an entry for each individual (sorted in any way). Each entry states the position of an individual. When the number of positions is naturally limited, such as when categorical positions are used, frequency vectors can summarize this information to calculate representation scores. A frequency has an entry for each position in the population (sorted in ascending order). Each entry states the number of individuals in the population with this position.

The argument \code{pos} is required if certain positions do not occur in the population. For example, if we have positions on a 7-point scale, and position 3 never occurs in the population, the argument \code{pos} must be specified. In this case, our argument may be \code{pos=1:7}. We can also use categories more generally, as in \code{c(-3,-1,0,.5,1,2,5)}.  Specifying the positions of categories when all positions actually occur in the population has no side-effects.

You can use the function \code{\link{expand}} to convert frequency vectors into their long form (population vector).}
\value{A frequency vector.}
\author{Didier Ruedin}
\seealso{\code{\link{expand}}}
