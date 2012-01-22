frequency.vector <-
function(D, k=9999919) {
   # Calculates a frequency vector F from a population vector D
   # Arguments: D = population vector
   #            k = number of categories (necessary if there are categories with 0)
   # NOTE: k = 9999919 is hard coded using a unrealistic value
   # e.g. if position 2 is not present in the population, no 0 will be added
   T <- as.data.frame(table(D))  # table(D) to count frequencies
   F <- T[,2]                    # [,2] chooses the values
   if (k == 9999919) k <- length(F) # if k is not provided, assume no categories with frequency 0
   if (!k == length(F)) {           # check if there are categories with 0 [length(F) not equal k]
     F2 <- F        # source of values
     F <- rep(0,k)  # empty with length(k)
     V <- unique(D) # chooses the categories (positions) of the values in F2
     j <- length(V) # number of non-zero values to be filled into F
     for(i in 1:j) {F[V[i]] <- F2[i]} # fill in values at the correct position
   }
   return(F)
   }
