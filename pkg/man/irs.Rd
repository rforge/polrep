\name{irs}
\alias{irs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to calculate individual representation scores.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
irs(P, R, perspective = "individual")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{
%%     ~~Describe \code{P} here~~
}
  \item{R}{
%%     ~~Describe \code{R} here~~
}
  \item{perspective}{
%%     ~~Describe \code{perspective} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (P, R, perspective = "individual") 
{
    if ((is.vector(P) & is.vector(R)) == FALSE) 
        stop("Error: expected two vectors (one for population, one for representatives).")
    n <- length(P)
    m <- length(R)
    if (perspective == "individual") 
        (Divide <- n - 1)
    else (Divide <- n)
    N <- NULL
    M <- NULL
    V <- NULL
    for (i in 1:n) {
        d <- 0
        for (j in 1:n) {
            d <- d + abs(P[i] - P[j])
        }
        N[i] <- d/Divide
    }
    for (i in 1:n) {
        d <- 0
        for (j in 1:m) {
            d <- d + abs(P[i] - R[j])
        }
        M[i] <- d/m
    }
    V <- N - M
    return(V)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
