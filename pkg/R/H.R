H <-
function(Z) {
  # Heterogeneity (after Fearon)
  z <- length(Z)
  1 - sum(Z[1:z]^2)
  }
