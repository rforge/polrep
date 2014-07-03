aacc <- function(Z,R) {
  # Absolute mean ("average")  citizen congruence
  #
  abs(mean(Z, na.rm=TRUE) - mean(R, na.rm=TRUE))
  }
