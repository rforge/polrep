\name{expand}
\alias{expand}
\title{Convert a frequency vector into a population vector}
\description{This function converts a frequency vector into a population vector.}
\usage{expand(F)}
\arguments{  \item{F}{Frequency vector}}
\details{This function converts a frequency vector into a population vector.  A frequency has an entry for each position in the population (sorted in ascending order). Each entry states the number of individuals in the population with this position. When the number of positions is naturally limited, such as when categorical positions are used, frequency vectors can summarize this information to calculate representation scores. A population vector has an entry for each individual (sorted in any way). Each entry states the position of an individual.

You can use the function \code{\link{frequency.vector}} to convert a population vector into its short form (frequency vector). The alias \code{\link{collapse}} can be used instead of \code{\link{frequency.vector}}.}
\value{A population vector.}
\author{Didier Ruedin}
\seealso{\code{\link{frequency.vector}}}
