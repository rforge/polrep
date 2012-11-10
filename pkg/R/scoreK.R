scoreK <- function(Z,R) {
  z <- length(Z)
  (1/z) * sum((R/Z)^2)
  }
