congruencePlot <- function(Z, R, from=NA, to=NA, steps=10000, Zcol=1, Rcol=1, Zlty=1, Rlty=2, Pcol=220, Pdensity=NULL, Pangle=45, ...) {
  # Plotting Congruence
  # are arguments provided, or do I have to infer them from the input strings?
  if (is.na(from))  {from <- 0}
  if (is.na(to))    {to   <- length(Z)}
  Zf <- ecdf(Z) # empirical CDF
  Rf <- ecdf(R) # empirical CDF
  r <- to - from
  z <- seq(from, to, by = r/steps)        # values at which to evaluate ecdf
  Zv <- Zf(z) # values for Z
  Rv <- Rf(z) # values for R
  plot(NULL, ylim=c(0,1), xlim=c(from, to), ...) # set titles, axes etc. here
  polygon(c(z, rev(z)), c(Zv, rev(Rv)), col=Pcol, density=Pdensity, angle=Pangle, border=NA)
  lines(Zv~z, col=Zcol, lty=Zlty)
  lines(Rv~z, col=Rcol, lty=Rlty)
  }
