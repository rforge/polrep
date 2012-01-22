\name{frequency.vector}
\alias{frequency.vector}
\title{Convert a population vector into a frequency vector}
\description{This function converts a population vector into a frequency vector. The alias \code{\link{collapse}} can also be used.}
\usage{frequency.vector(D,k=9999919)}
\arguments{
  \item{D}{Population vector}
  \item{k}{Optional: number of categories}
}
\details{This function converts a population vector into a frequency vector. A population vector has an entry for each individual (sorted in any way). Each entry states the position of an individual. When the number of positions is naturally limited, such as when categorical positions are used, frequency vectors can summarize this information to calculate representation scores. A frequency has an entry for each position in the population (sorted in ascending order). Each entry states the number of individuals in the population with this position.

The argument \code{k} is necessary if certain positions do not occur in the population. For example, if we have positions on a 7-point scale, and position 3 never occurs in the population, the argument \code{k} must be specified. The default value of k is hard-coded to a number extremely unlikely to reflect empirical data. Specifying the number of categories when all positions actually occur in the population has no side-effects.

You can use the function \code{\link{expand}} to convert frequency vectors into their long form (population vector).}
\value{A frequency vector.}
\author{Didier Ruedin}
\seealso{\code{\link{expand}}}
