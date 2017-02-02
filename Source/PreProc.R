BaselineRecovery <- function(){
  ecgInfartosBL <<-  lapply(ecgInfartos, msSmoothMRD, wavelet="d6", levels=5,xform="dwt")
  ecgSanosBL <<-  lapply(ecgSanos, msSmoothMRD, wavelet="d6", levels=5,xform="dwt")

  for (i in names(ecgInfartos)){
    ecgInfartosBL[[i]] <<- ecgInfartos[[i]]-ecgInfartosBL[[i]]
  }
  for (i in names(ecgSanos)){
    ecgSanosBL[[i]] <<- ecgSanos[[i]]-ecgSanosBL[[i]]
  }
}

FiltrarMuestras <- function() {
  BaselineRecovery()
  ecgInfartosF <<- lapply(ecgInfartosBL, wavShrink, wavelet="d6",shrink.fun="soft", thresh.fun="universal", thresh.scale=1.75, xform="dwt",n.level=5)  
  ecgSanosF <<- lapply(ecgSanosBL, wavShrink, wavelet="d6",shrink.fun="soft", thresh.fun="universal", thresh.scale=1.75, xform="dwt",n.level=5)
}


HRV <- function() {
  RRInf <<- list()
  RRSanos <<- list()  
  hrvInf <<- list()
  hrvSanos <<- list()
  
  ### Sequential
  maxInfartos <<- lapply(ecgInfartosF, msExtrema,span=250)
  maxSanos <<- lapply(ecgSanosF, msExtrema,span=250)  
  
  ### Parallel
  #maxInfartos <- mclapply(ecgInfartosF, msExtrema,span=185, mc.cores=2)
  #maxSanos <<- mclapply(ecgSanosF, msExtrema,span=185, mc.cores=2)
  
  for (i in names(maxInfartos)){
    N <- length((seq.int(from=0,to=300,by=1/250))[maxInfartos[[i]]$index.max])
    n = 2:N;  
    RRInf[[i]] <<- (seq.int(from=0,to=300,by=1/250))[maxInfartos[[i]]$index.max][n+1]-
      (seq.int(from=0,to=300,by=1/250))[maxInfartos[[i]]$index.max][n]  
  }
  
  for (i in names(maxSanos)){
    N <- length((seq.int(from=0,to=300,by=1/250))[maxSanos[[i]]$index.max])
    n = 2:N;  
    RRSanos[[i]] <<- (seq.int(from=0,to=300,by=1/250))[maxSanos[[i]]$index.max][n]-
      (seq.int(from=0,to=300,by=1/250))[maxSanos[[i]]$index.max][n-1]  
  }
  
  for (i in names(RRInf)){
    N <- length(RRInf[[i]])
    n = 1:N  
    hrvInf[[i]] <<- (1/RRInf[[i]][n])*60  
  }
  
  for (i in names(RRSanos)){
    N <- length(RRSanos[[i]])
    n = 1:N
    hrvSanos[[i]] <<- (1/RRSanos[[i]][n])*60    
  }
  
  ### Validación de la Frecuencia Cardiaca de los sujetos Sanos
  for (k in 1:length(RRSanos)){
    u_ult <- 13
    u_med <- 1.5*13
    LONG <- 50
    MINI <- 12
    MAXI <- 20
    FIJO <- 10
    L <- 1    
    n <- length(RRSanos[[k]])
    for (i in 2:n-1){
      if (i < LONG){
        M <- mean(RRSanos[[k]][i:1])
      }
      else {
        M <- mean(RRSanos[[k]][i:(i-50)])
      }    
      if (100*abs(hrvSanos[[k]][i] - hrvSanos[[k]][L])/hrvSanos[[k]][L] < u_ult ||
        100*abs(hrvSanos[[k]][i] - hrvSanos[[k]][i + 1])/hrvSanos[[k]][L] < u_ult  ||
        100*abs(hrvSanos[[k]][i] - M )/M < u_med){
        #se acepta el latido i
        hrvSanos[[k]][i] <<- hrvSanos[[k]][i]            
        L <- i      
        if (i %% LONG == 0){
          tmp <- FIJO + sd(hrvSanos[[k]][i:(i-LONG)])
          if (tmp < MINI){
            tmp = MINI
          }
          if (tmp > MAXI){
            tmp <- MAXI
          }
          u_ult <- tmp
          u_med <- 1:5*tmp          
        }
      }      
      else {
        i = i + 1
      }
    }
  }
  
  
  ### Validación de la Frecuencia Cardiaca de los Pacientes Enfermos
  for (k in 1:length(RRInf)){
    u_ult <- 13
    u_med <- 1.5*13
    LONG <- 50
    MINI <- 12
    MAXI <- 20
    FIJO <- 10
    L <- 1    
    n <- length(RRInf[[k]])
    for (i in 2:n-1){
      if (i < LONG){
        M <- mean(RRInf[[k]][i:1])
      }
      else {
        M <- mean(RRInf[[k]][i:(i-50)])
      }    
      if (100*abs(hrvInf[[k]][i] - hrvInf[[k]][L])/hrvInf[[k]][L] < u_ult ||
        #100*abs(hrvInf[[k]][i] - hrvInf[[k]][i + 1])/hrvInf[[k]][L] < u_ult  ||
        100*abs(hrvInf[[k]][i] - M )/M < u_med){
        #se acepta el latido i
        hrvInf[[k]][i] <- hrvInf[[k]][i]            
        L <- i      
        if (i %% LONG == 0){
          tmp <- FIJO + sd(hrvInf[[k]][i:(i-LONG)])
          if (tmp < MINI){
            tmp = MINI
          }
          if (tmp > MAXI){
            tmp <- MAXI
          }
          u_ult <- tmp
          u_med <- 1:5*tmp          
        }
      }      
      else {
        i = i + 1
      }
    }
  }
}
       
