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

#B.Forutsetninger 
K<- 100 #Strike price
T <- 1  # Tid i  år
S_0 <- 100 #Pris på aksje i periode 0
ExpVol<- 0.3      # Forventet volatilitet
d <-0.00        # Forventet utbytte kontinuerlig
Rf<-0.05;   # Risikofri rente
itr<-1000# Antall iterasjoner
N<-itr*1000
Periode<-365 #Antall perioder per år
drifts<-Periode*T