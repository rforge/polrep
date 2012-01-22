R.score <-
function(Z,R) {
  # (assumes majority group in position 1)
  # z <- length(Z)
  # r <- length(R)
  # sum(R[2:r]) / sum(Z[2:z])
  sum(R[-1]) / sum(Z[-1])
  }
