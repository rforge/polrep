sortVectors <- function(Z,R) {
	# function to sort two vectors
	if (length(Z) != length(R)) stop("Error: Vectors do not have same length.") # input validation
	# put into data.frame
	D <- as.data.frame(rbind(Z,R))
	# sort data.frame
	S <- D[order(D[1,], decreasing=TRUE)]
	# output separately
	return(list(Z=as.numeric(S[1,]), R=as.numeric(S[2,])))
	}
