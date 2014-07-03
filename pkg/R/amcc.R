amcc <- function(Z,R) {
  # Absolute median citizen congruence
  #
  abs(median(Z, na.rm=TRUE) - median(R, na.rm=TRUE))
  }
