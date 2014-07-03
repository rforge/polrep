accm <- function(Z,R) {
  # Absolute citizen congruence, using the mean
  z <- length(Z)
  (1/z) * sum(abs(Z - mean(R, na.rm=TRUE)))
  }
