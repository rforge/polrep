GTT <- function (x) {
# calculate two-partiness (Gains & Taagepera T)
# input: vector of vote share, ordered by party strength
# input validation: are percentages provided?
s <- sum(x)
if (s>1) {x <- x/100
warning("Sum of the vector provided is greater than 1: Values were divided by 100, assuming percentages were provided. Check your input.")
}
# two-partiness:
P1 <- x[1]
P2 <- x[2]
P3 <- x[3]
return((P2-P3)*(P1+P2)/P1)
}
