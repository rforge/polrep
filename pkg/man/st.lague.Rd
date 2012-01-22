\name{st.lague}
\alias{st.lague}
\title{Sainte-Lague}
\description{Calculates Sainte-Lague.}
\usage{st.lague(Z,R)}
\arguments{
  \item{Z}{Population vector}
  \item{R}{Vector for the representatives}
}
\details{The representation score is calculated as the Sainte-Lague method: sum((Z-R)^2/R).}
\value{Representation score}
\author{Didier Ruedin}
\seealso{\code{\link{st.lague.inverted}}}
