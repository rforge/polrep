neffProd <- function (x) {
# effective number of parties (product)
# input validation: are percentages provided?
s <- sum(x)
if (s>1) {x <- x/100
warning("Sum of the vector provided is greater than 1: Values were divided by 100, assuming percentages were provided. Check your input.")
}
# Neff:
return(1/prod(x^x))
}
