relrep <- function(Z, R, k=1) {
  # k as the position of the group under study (e.g. majority group)
  # no input validation, so that we can use ad-hoc combinations like k=1:3
  # if k is less than 1 or greater than the number of groups, set it to 1 (invalid input)
  # if (k>length(Z) | k<1) {
  #   warning("Warning: Invalid group, assumes group in position 1")
  #  k <- 1
  # }
  sum(R[-k]) / sum(Z[-k])
  }
