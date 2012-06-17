cosScore <- function(Z,R) {
sum(Z*R)/(sum(Z^2)*sum(R^2))^.5
}
