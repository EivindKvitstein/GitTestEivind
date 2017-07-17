#European Option-----------------------------------------------

#Installer data table package
#install.packages("data.table")
#Last inn data table package
library(data.table)

# A. Black-Scholes Option Value
# Call value is returned in values[1], put in values[2]
BlackScholes <- function(S, K, Rf, T, sigma,d) {
  values <- c(2)
  
  d1 <- (log(S/K)+(Rf-d+sigma^2/2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  values[1] <- S*exp(-d*T)*pnorm(d1) - K*exp(-Rf*T)*pnorm(d2)
  values[2] <- K*exp(-Rf*T) * pnorm(-d2) - S*exp(-d*T)*pnorm(-d1)
  
  values
}
