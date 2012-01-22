R.score.max <-
function(Z,R) {
  # z <- length(Z)
  # r <- length(R)
  # max(R[2:r]/Z[2:z])
  max(R[-1]/Z[-1])
  }
