rcc <- function(Z,R) {
  # Relative citizen congruence
  1 - (sum(abs(Z - median(Z, na.rm=TRUE)))/(sum(abs(Z - median(R, na.rm=TRUE)))))
  }
