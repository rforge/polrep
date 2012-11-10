\name{relrep}
\alias{relrep}
\title{Calculates relative representation scores}
\description{Calculates relative representation scores based on a population vector and a vector for the representatives.}
\usage{relrep(Z,R,k=1)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
  \item{k}{Identify group for comparison}
}
\details{Relative representation scores compare one group with all other groups combined. For instance, we can compare the majority group with all minority groups combined into one group. Relative representation scores calculate the proportion of the combined minorities in the representatives by the proportion of the combined minorities in the population. Unless specified, the majority group must be in first position of the vector. All groups in positions 2 to n are considered minority groups. No test is applied to check whether the groups are numerical minorities, but you can specify any other group with a value of k.}
\value{Representation score}
\author{Didier Ruedin}
\seealso{\code{\link{relrepMax}}}
