irs.cat <-
function(G, H, perspective="individual") {
  # Function to calculate IRS for discrete values (as in the Gnumeric sheet)
  # Arguments: G = frequency vector for the population
  #            H = frequency vector for the representatives
  #  perspective = perspective of the individual (default) or the population
  # Note: A frequency vector describes how many individuals have the issue position 1, how many issue position 2, ...
  # Note: The distance between each position is treated as equal.
  # Example: 3 citizens with position 1, 5 citizens with position 2, 2 citizens with position 3: F <- c(3,5,2)

  # Check: Are both F and G vectors?
  if ((is.vector(G) & is.vector(H)) == FALSE) stop("Error: expected two vectors (one for population, one for representatives).")
  if (min(G) < 0) stop("Error: negative values found in frequency vector for the population.") # negative frequencies
  if (min(H) < 0) stop("Error: negative values found in frequency vector for the representatives.") # negative frequencies
  # Get the number of categories (t)
  t <- length(G)
  # Check that frequency vectors are equal length
  if ((t == length(H)) == FALSE) stop("Error: Vectors are not of same length.")
  # Get the number of citizens (n) and representatives (m)
  n <- sum(G)
  m <- sum(H)
  # Constant according to perspective
  if (perspective == "individual") (Divide <- n-1) else (Divide <- n)
  # New (empty) vectors for marginality and IRS
  N <- NULL
  M <- NULL
  V <- NULL
  # Calculate marginality among population [N] for each position of the citizens
  for (i in 1:t) {   # repeat for each position
    d <- 0           # begin with no distance
    for (j in 1:t) { # compare to each of t positions
      d <- d + G[j]*abs(i-j)  # summing up distances
    }
    N[i] <- d/Divide  # average distance, not counting self (n-1) for individual perspective, counting self (n) otherwise
  }
  # Calculate marginality among representatives [M] for each position of the citizens
  for (i in 1:t) {   # repeat for each position
    d <- 0           # begin with no distance
    for (j in 1:t) { # compare to each of t positions
      d <- d + H[j]*abs(i-j)  # summing up distances
    }
    M[i] <- d/m      # average distance
  }
  V <- N - M         # IRS for each position
  return(V)          # reporting IRS
}
