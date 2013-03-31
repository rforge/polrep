GTD2 <- function (x) {
# calculate two-partiness (Gains & Taagepera D2)
# input: vector of vote share, ordered by party strength
# input validation: are percentages provided?
s <- sum(x)
if (s>1) {x <- x/100
warning("Sum of the vector provided is greater than 1: Values were divided by 100, assuming percentages were provided. Check your input.")
}
# two-partiness:
k <- length(x)
two.largest <- x[1:2]
others      <- x[3:k]
A <- sum((0.5 - two.largest)^2)
B <- sum((0.0 - others)^2)
return(1-sqrt(2)*sqrt(A+B))
}
