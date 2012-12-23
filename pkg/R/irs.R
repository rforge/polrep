irs <- function(P, R, perspective="individual") {
  # Calculates IRS (generic)
  # Arguments: P = vector P for the population, issue position of each citizen
  #            R = vector R for the representatives, issue position of each representative
  #  perspective = perspective of the individual (default) or the population
  # Example: Citizen 1 with position 5, citizen 2 with position 3, citizen 3 with position 1: P <- c(5,3,1)

  # Input validation: Are both P and R vectors?
  if ((is.vector(P) & is.vector(R)) == FALSE) stop("Error: expected two vectors (one for population, one for representatives).")
  # How many citizens and how many representatives are there?
  n <- length(P) # number of citizens: n
  m <- length(R) # number of representatives: m
  # Constant according to perspective
  if (perspective == "individual") (Divide <- n-1) else (Divide <- n)
  # New (empty) vectors for marginality and IRS
  N <- NULL
  M <- NULL
  V <- NULL
  # Calculate marginality among population for each individual
  for (i in 1:n) {   # repeat for each of n individuals
    d <- 0           # begin with no distance
    for (j in 1:n) { # compare to each of n individuals
      # No need to exclude self, since distance to self is 0
      d <- d + abs(P[i]-P[j]) # summing up distances
    }
  # Vector N to store individual marginality
  N[i] <- d/Divide    # average distance, not counting self (n-1) for individual perspective, counting self (n) otherwise
  }
  # Calculate marginality among representatives for each individual
  for (i in 1:n) {   # repeat for each of n individuals
    d <- 0           # begin with no distance
    for (j in 1:m) { # compare to each of m representatives
      d <- d + abs(P[i]-R[j]) # summing up distances
    }
    M[i] <- d/m      # average distance
  }
  V <- N - M         # IRS
  return(V)          # reporting IRS
}
