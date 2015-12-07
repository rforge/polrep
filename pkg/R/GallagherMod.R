GallagherMod <- function(Z, R) {
# modified Gallagher index
	# x'=x/sum(xi^2)^.5
	Z.div <- sum(Z^2)^0.5
	Z.mod <- Z/Z.div
	# y'=y/sum(yi^2)^.5
	R.div <- sum(R^2)^0.5
	R.mod <- R/R.div
	sqrt(sum((Z.mod-R.mod)^2)/2)
  }
