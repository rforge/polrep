Zc <- function(Z) {
  # Proportion of minorities in population (all groups except first)
  z <- length(Z)
  sum(Z[2:z])
  }
