acc <- function(Z,R) {
  # Absolute citizen congruence
  z <- length(Z)
  (1/z) * sum(abs(Z - median(R, na.rm=TRUE)))
  }
