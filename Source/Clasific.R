ClassAgnes <- function(){
  ag <<- agnes(CDMMat,diss=TRUE,method="single")      
  #plot(ag, ask = FALSE, which.plots = NULL,main="CDM de ECG Infartados Vs. Sanos", ylab="Altura", xlab="Medida CDM")
}


ClassDiana <- function(){ 
  di <<- diana(CDMMat,diss=TRUE)      
  #plot(ag, ask = FALSE, which.plots = NULL,main="CDM de ECG Infartados Vs. Sanos", ylab="Altura", xlab="Medida CDM")
}
