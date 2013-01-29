CoxShugart <- function(Z,R) {
  # m <- lm (z ~ x)
  # a <- as.numeric(m$coefficients[1])
  # b <- as.numeric(m$coefficients[2])
  RR <- mean(R)
  ZZ <- mean(Z)
  b <- sum((R-RR) * (Z-ZZ))/sum((Z-ZZ)^2)
  return(b)
  }
