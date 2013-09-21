irs.plot <- function(G, H, perspective="individual", groups=c("Voters","MP"), concept="Ideology") {
  # Function to plot IRS for discrete values
  # uses irs.cat to calculte IRS
  # Arguments: G = frequency vector for the population
  #            H = frequency vector for the representatives
  #  perspective = perspective of the individual (default) or the population
  # no sanity checking here; errors will be thrown by irs.cat, if any
  par(mar=c(2,4,2,2)) # borders
  split.screen(c(2,1))           # screens 1 and 2
  split.screen(c(1,2), screen=1) # screens 3 and 4 (replacing screen 1)
    screen(3)
      plot(G, type="h", axes=FALSE, ylab=groups[1], xlab="")
      axis(1); axis(2)
    screen(4)
      plot(H, type="h", axes=FALSE, ylab=groups[2], xlab="")
      axis(1); axis(2)
    screen(2)
      par(mar=c(4,4,2,3)) # borders
      plot(irs.cat(G,H,perspective=perspective), type="h", axes=FALSE, ylab="IRS", xlab=concept)
      axis(1); axis(2)
      abline(h=0, col="grey")
  close.screen(all.screens=TRUE)
}
