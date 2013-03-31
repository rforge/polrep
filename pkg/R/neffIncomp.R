neffIncomp <- function (x) {
  # effective number of parties for incomplete data
  # assumes residual category to be the last item!
  P <- sum(x)                       # number of seats
  R <- x[length(x)]                 # last item = residual category (assumed!)
  A <- x[1:length(x)-1]             # exclude last item ( = exclude residual)
  PL <- minnz(A)                    # smallest non-zero component reported
  # requires minnz() from agrmt package (non-zero minimum)
  noN <- P^2/(R + sum(A^2))         # f(R) = R, residual not squared ("simple")
  lowN <- P^2/(R^2 + sum(A^2))      # f(R) = R^2 ( = residual included)
  highN <- P^2/(sum(A^2))           # f(R) = 0, residual excluded
  leastN <- P^2/(R*PL + sum(A^2))   # f(R) = least component
  M <- ifelse (min(R^2, R*PL) == R*PL, leastN, lowN) # lower of R ^2 and PL
  B <- mean(c(highN,M))             # best estimate
  r <- list(Neff = B,  simple = noN, low = lowN, least = leastN, high = highN)
  return(r)
}
