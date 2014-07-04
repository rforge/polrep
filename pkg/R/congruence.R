congruence <- function(Z, R, from=NA, to=NA, steps=10000) {
  # Congruence
  # are arguments provided, or do I have to infer them from the input strings?
  if (is.na(from))  {from <- 0}
  if (is.na(to))    {to   <- length(Z)}
  Zf <- ecdf(Z) # empirical CDF
  Rf <- ecdf(R) # empirical CDF
  r <- to - from
  z <- seq(from, to, by = r/steps)        # values at which to evaluate ecdf
  a <- sum(abs(Zf(z) - Rf(z)))/length(z)  # area between the two curves (approximated)
  return(a)
  }
