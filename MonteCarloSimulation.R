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
T <- 2  # Tid i  år
S_0 <- 100 #Pris på aksje i periode 0
ExpVol<- 0.3      # Forventet volatilitet
d <-0.00        # Forventet utbytte kontinuerlig
Rf<-0.05;   # Risikofri rente
itr<-1000# Antall iterasjoner
N<-itr*1000
Periode<-365 #Antall perioder per år
drifts<-Periode*T


#C.Forkalkulasjoner 
dt<- 1/Periode 
nudt<- (Rf - d - 0.5 * (ExpVol^ 2))* dt
sigsdt<-ExpVol * sqrt(dt)

#D.DataTable
Names<-as.character(paste0("Simulering",1:itr))
m<-matrix(0,nrow = drifts,ncol=itr)
Simulering<-data.table(m) 
m<-NULL
setnames(Simulering,Names)
Simulering[,Tid:=1:drifts]

# create progress bar
pb <- winProgressBar(title = "progress bar", min = 0,
                     max = itr,width = 300)

#E. Simulering-> lag illustrasjon
for( i in (1:itr))
{
  dS<-S_0
  
  for (j in (1:drifts))
  {
    eps<-rnorm(1, mean = 0, sd = 1)
    dS<-dS * exp(nudt +sigsdt*eps)
    set(Simulering,j,i,dS)
    setWinProgressBar(pb, i, title=paste( round(i/itr*100, 0),
                                          "% done"))
  }
  
}
close(pb)

PayoffCall<-unlist(ifelse(Simulering[drifts,!"Tid"]-K>0,Simulering[drifts,!"Tid"]-K,0))
PayoffCall<-PayoffCall*exp(-T*Rf)
PayoffPut<-unlist(ifelse(K-Simulering[drifts,!"Tid"]>0,K-Simulering[drifts,!"Tid"],0))
PayoffPut<-PayoffPut*exp(-T*Rf)
#Set tid først
setcolorder(Simulering,c("Tid",paste0("Simulering",1:itr)))
#F.Plot Resultater


#G.Rask simulering


#H.Resultater
Call=mean(PayoffCall)
Call
Put=mean(PayoffPut)
Put
BlackScholes(S_0,K,Rf,T,ExpVol,d)

