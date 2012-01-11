\name{irs.cat}
\alias{irs.cat}
\title{Calculate individual representation scores for categorical data }
\description{This function calculates individual representation scores based on frequency vectors. If the data are in this form, this function is much faster than the standard IRS function.}
\usage{
irs.cat(G, H, perspective = "individual")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{G}{
%%     ~~Describe \code{G} here~~
}
  \item{H}{
%%     ~~Describe \code{H} here~~
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
function (G, H, perspective = "individual") 
{
    if ((is.vector(G) & is.vector(H)) == FALSE) 
        stop("Error: expected two vectors (one for population, one for representatives).")
    if (min(G) < 0) 
        stop("Error: negative values found in frequency vector for the population.")
    if (min(H) < 0) 
        stop("Error: negative values found in frequency vector for the representatives.")
    t <- length(G)
    if ((t == length(H)) == FALSE) 
        stop("Error: Vectors are not of same length.")
    n <- sum(G)
    m <- sum(H)
    if (perspective == "individual") 
        (Divide <- n - 1)
    else (Divide <- n)
    N <- NULL
    M <- NULL
    V <- NULL
    for (i in 1:t) {
        d <- 0
        for (j in 1:t) {
            d <- d + G[j] * abs(i - j)
        }
        N[i] <- d/Divide
    }
    for (i in 1:t) {
        d <- 0
        for (j in 1:t) {
            d <- d + H[j] * abs(i - j)
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
