par(mfrow=c(1,2))
hist.default(hrvInf[[4]],breaks=10, freq=TRUE,xlab="VFC", ylab="Frecuencia",main="Paciente con Infarto")
hist.default(hrvSanos[[4]],breaks=10, freq=TRUE,xlab="VFC", ylab="Frecuencia", main="Sujeto Sano")
dev.off()

Time <- c(4.894,21.381,83.514,214.945,433.087,860.878,1258.695)
ts.plot(Time)
#Temp <- c(ZSAXInf[1],ZSAXInf[2],ZSAXInf[3],ZSAXInf[4],ZSAXInf[5],ZSAXSanos[6],ZSAXSanos[7],ZSAXSanos[8],ZSAXSanos[9],ZSAXSanos[10])
#Temp <- c(SAXInf[[1]],SAXInf[[2]],SAXInf[3],SAXInf[4],SAXInf[5],SAXSanos[6],SAXSanos[7],SAXSanos[8],SAXSanos[9],SAXSanos[10])
N <- 10

CDMMat <- matrix(nrow=N,ncol=N)
EucMat <<- matrix(nrow=N,ncol=N)

for (i in 1:N){
  write(Temp[[i]], file="TempA.txt",ncolumns=1,sep="")
  system("gzip -f TempA.txt")
  for (j in 1:N){
    write(Temp[[j]], file="TempB.txt",ncolumns=1,sep="")
    write(c(Temp[[j]], Temp[[i]]), file="TempAB.txt",ncolumns=1,sep="")
    system("gzip -f TempB.txt")
    system("gzip -f TempAB.txt")
    CDMMat[i, j] <- file.info("TempAB.txt.gz")$size/
      (file.info("TempA.txt.gz")$size+
      file.info("TempB.txt.gz")$size)
  }
}

colnames(CDMMat)[1] <- c("I")
rownames(CDMMat) <- names(Temp)

system("ls -al | grep Temp")
system("rm Temp*")

for (i in 1:N){
  for (j in 1:N){
    EucMat[i,j] <- euclidean(PAAInf[[i]], PAASanos[[j]])
  }
}
colnames(EucMat) <- names(Temp)
rownames(EucMat) <- names(Temp)


mdInfTime <- data.frame()
  for (i in names(mdInf)){
    mdInfTime[i,1] <- mdInf[[i]]$TimeAnalysis[[1]]$size
    mdInfTime[i,2] <- mdInf[[i]]$TimeAnalysis[[1]]$SDNN
    mdInfTime[i,3] <- mdInf[[i]]$TimeAnalysis[[1]]$SDANN
    mdInfTime[i,4] <- mdInf[[i]]$TimeAnalysis[[1]]$SDNNIDX
    mdInfTime[i,5] <- mdInf[[i]]$TimeAnalysis[[1]]$pNN50
    mdInfTime[i,6] <- mdInf[[i]]$TimeAnalysis[[1]]$rMSSD
    mdInfTime[i,7] <- mdInf[[i]]$TimeAnalysis[[1]]$IRRR
    mdInfTime[i,8] <- mdInf[[i]]$TimeAnalysis[[1]]$MADRR
    mdInfTime[i,9] <- mdInf[[i]]$TimeAnalysis[[1]]$TINN
    mdInfTime[i,10] <- mdInf[[i]]$TimeAnalysis[[1]]$HRVi
  }
colnames(mdInfTime) <- c("Tiempo (sec)","SDNN","SDANN","SDNNIDX","pNN50",
                         "rMSSD","IRRR","MADRR","TINN","HRVi")
write.table(mdInfTime,file="mdInfTime.csv",sep = ",",col.names = NA,qmethod = "double")


mdSanTime <- data.frame()
for (i in names(mdSan)){
  mdSanTime[i,1] <- mdSan[[i]]$TimeAnalysis[[1]]$size
  mdSanTime[i,2] <- mdSan[[i]]$TimeAnalysis[[1]]$SDNN
  mdSanTime[i,3] <- mdSan[[i]]$TimeAnalysis[[1]]$SDANN
  mdSanTime[i,4] <- mdSan[[i]]$TimeAnalysis[[1]]$SDNNIDX
  mdSanTime[i,5] <- mdSan[[i]]$TimeAnalysis[[1]]$pNN50
  mdSanTime[i,6] <- mdSan[[i]]$TimeAnalysis[[1]]$rMSSD
  mdSanTime[i,7] <- mdSan[[i]]$TimeAnalysis[[1]]$IRRR
  mdSanTime[i,8] <- mdSan[[i]]$TimeAnalysis[[1]]$MADRR
  mdSanTime[i,9] <- mdSan[[i]]$TimeAnalysis[[1]]$TINN
  mdSanTime[i,10] <- mdSan[[i]]$TimeAnalysis[[1]]$HRVi
}
colnames(mdSanTime) <- c("Tiempo (sec)","SDNN","SDANN","SDNNIDX","pNN50",
                         "rMSSD","IRRR","MADRR","TINN","HRVi")
write.table(mdSanTime,file="mdSanTime.csv",sep = ",",col.names = NA,qmethod = "double")


ptm <- proc.time()
for (i in 1:300){
  SAXInf[[i]] <- ts2string(PAAInf[[i]],aSize=10)
}
proc.time() - ptm



RHRVfun <- function(){
  mdInf <<- list()
  for (i in names(maxInfartos)){  
    mdInf[[i]] <<- CreateHRVData(Verbose=FALSE)
    write(seq.int(from=0,to=300,by=1/250)[1:75000][maxInfartos[[i]]$index.max],file="Temp.txt",ncolumns=1,sep="")
    mdInf[[i]] <<- LoadBeatAscii(mdInf[[i]],FileName="./Temp.txt",verbose=FALSE)
    system("rm Temp*")
    mdInf[[i]] <<- BuildNIHR(mdInf[[i]],verbose=FALSE)
    mdInf[[i]] <<- InterpolateNIHR(mdInf[[i]],verbose=FALSE)  
    mdInf[[i]] <<- FilterNIHR(mdInf[[i]], verbose=FALSE)
    mdInf[[i]] <<- CreateTimeAnalysis(mdInf[[i]], verbose=FALSE)  
    mdInf[[i]] <<- CreateFreqAnalysis(mdInf[[i]], verbose=FALSE)
    mdInf[[i]] <<- CreateNonLinearAnalysis(mdInf[[i]], verbose=FALSE)
  }
  mdSan <<- list()
  for (i in names(maxSanos)){  
    mdSan[[i]] <<- CreateHRVData(Verbose=FALSE)
    write(seq.int(from=0,to=300,by=1/250)[1:75000][maxSanos[[i]]$index.max],file="Temp.txt",ncolumns=1,sep="")
    mdSan[[i]] <<- LoadBeatAscii(mdSan[[i]],FileName="./Temp.txt",verbose=FALSE)
    system("rm Temp*")
    mdSan[[i]] <<- BuildNIHR(mdSan[[i]],verbose=FALSE)
    mdSan[[i]] <<- InterpolateNIHR(mdSan[[i]],verbose=FALSE)
    mdSan[[i]] <<- FilterNIHR(mdSan[[i]], verbose=FALSE)
    mdSan[[i]] <<- CreateTimeAnalysis(mdSan[[i]], verbose=FALSE)  
  }
}


ecgInfartosBL <-  lapply(ecgInfartos, msSmoothMRD, wavelet="d6", levels=5,xform="dwt")
ecgSanosBL <-  lapply(ecgSanos, msSmoothMRD, wavelet="d6", levels=5,xform="dwt")

for (i in names(ecgInfartos)){
  ecgInfartosBL[[i]] <- ecgInfartos[[i]]-ecgInfartosBL[[i]]
}
for (i in names(ecgSanos)){
  ecgSanosBL[[i]] <- ecgSanos[[i]]-ecgSanosBL[[i]]
}

ecgInfartosF <- lapply(ecgInfartosBL, wavShrink, wavelet="d6",shrink.fun="soft", thresh.fun="universal", thresh.scale=1.75, xform="dwt",n.level=5)  
ecgSanosF <- lapply(ecgSanosBL, wavShrink, wavelet="d6",shrink.fun="soft", thresh.fun="universal", thresh.scale=1.75, xform="dwt",n.level=5)

maxInfartos <- lapply(ecgInfartosF[1:10],msExtrema,span=250)
maxSanos <- lapply(ecgSanosF[1:10], msExtrema,span=250)


RRInf <- list()
RRSanos <- list()  
hrvInf <- list()
hrvSanos <- list()




for (i in names(maxInfartos)){
  N <- length((seq.int(from=0,to=300,by=1/250))[maxInfartos[[i]]$index.max])
  n = 2:N;  
  RRInf[[i]] <- (seq.int(from=0,to=300,by=1/250))[maxInfartos[[i]]$index.max][n+1]-
    (seq.int(from=0,to=300,by=1/250))[maxInfartos[[i]]$index.max][n]  
}

for (i in names(maxSanos)){
  N <- length((seq.int(from=0,to=300,by=1/250))[maxSanos[[i]]$index.max])
  n = 2:N;  
  RRSanos[[i]] <- (seq.int(from=0,to=300,by=1/250))[maxSanos[[i]]$index.max][n]-
    (seq.int(from=0,to=300,by=1/250))[maxSanos[[i]]$index.max][n-1]  
}


for (i in names(RRInf)){
  N <- length(RRInf[[i]])
  n = 1:N  
  hrvInf[[i]] <- (1/RRInf[[i]][n])*60  
}

for (i in names(RRSanos)){
  N <- length(RRSanos[[i]])
  n = 1:N
  hrvSanos[[i]] <- (1/RRSanos[[i]][n])*60    
}


for (k in 1:length(RRSanos)){  
  n <- length(RRSanos[[k]])
  U <- 0.5
  
  #Filtro A
  TempA <- 0
  j <- 1  
  for (i in 2:n){
    if (1 - U < RRSanos[[k]][i]/RRSanos[[k]][i - 1] &&
        RRSanos[[k]][i]/RRSanos[[k]][i - 1] < 1 + U){
      TempA[j] <- RRSanos[[k]][i]
      j = j + 1
    }
  }
  
  #Filtro B 
  for (i in 2:(n - 1)){
    if ((1 - U < RRSanos[[k]][i]/RRSanos[[k]][i - 1] &&
      RRSanos[[k]][i]/RRSanos[[k]][i - 1] < 1 + U) || 
      (1 - U < RRSanos[[k]][i]/RRSanos[[k]][i + 1] &&
      RRSanos[[k]][i]/RRSanos[[k]][i + 1] < 1 + U)){
      RRSanos[[k]] <- c(RRSanos[[k]], RRSanos[[k]][i])
    }    
  }
  
#   
#   #Filtro C
#   for (i in 2:(n-1)){
#     if ((1 - U < RRSanos[[k]][i]/RRSanos[[k]][i - 1] &&
#       RRSanos[[k]][i]/RRSanos[[k]][i - 1] < 1 + U) &&
#       (1 - U < RRSanos[[k]][i]/RRSanos[[k]][i + 1] &&
#       RRSanos[[k]][i]/RRSanos[[k]][i + 1] < 1 + U)){
#       RRSanos[[k]] <- c(RRSanos[[k]], RRSanos[[k]][i])
#     }    
#   }
  
  #Filtro D    
#   for (i in 1:n){
#     if ((1 - U < RRSanos[[k]][n]/RRSanos[[k]][i - 1] &&
#       RRSanos[[k]][n]/RRSanos[[k]][i - 1] < 1 + U) && 
#       (1 - U < RRSanos[[k]][n]/RRSanos[[k]][i + 1] &&
#       RRSanos[[k]][n]/RRSanos[[k]][i + 1] < 1 + U)){
#       RRSanos[[k]] <- c(RRSanos[[k]], RRSanos[[k]][i])
#     }    
#   }
}



for (k in 1:length(RRInf)){  
  n <- length(RRInf[[k]])
  U <- 0.5
  
  #Filtro A
  TempA <- 0
  j <- 1  
  for (i in 2:n){
    if (1 - U < RRInf[[k]][i]/RRInf[[k]][i - 1] &&
      RRInf[[k]][i]/RRInf[[k]][i - 1] < 1 + U){
      TempA[j] <- RRInf[[k]][i]
      j = j + 1
    }
  }
  
  #Filtro B 
  for (i in 2:(n - 1)){
    if ((1 - U < RRInf[[k]][i]/RRInf[[k]][i - 1] &&
      RRInf[[k]][i]/RRInf[[k]][i - 1] < 1 + U) || 
      (1 - U < RRSanos[[k]][i]/RRSanos[[k]][i + 1] &&
      RRInf[[k]][i]/RRInf[[k]][i + 1] < 1 + U)){
      RRInf[[k]] <- c(RRInf[[k]], RRInf[[k]][i])
    }    
  }
  
  #   
  #   #Filtro C
  #   for (i in 2:(n-1)){
  #     if ((1 - U < RRSanos[[k]][i]/RRSanos[[k]][i - 1] &&
  #       RRSanos[[k]][i]/RRSanos[[k]][i - 1] < 1 + U) &&
  #       (1 - U < RRSanos[[k]][i]/RRSanos[[k]][i + 1] &&
  #       RRSanos[[k]][i]/RRSanos[[k]][i + 1] < 1 + U)){
  #       RRSanos[[k]] <- c(RRSanos[[k]], RRSanos[[k]][i])
  #     }    
  #   }
  
  #Filtro D    
  #   for (i in 1:n){
  #     if ((1 - U < RRSanos[[k]][n]/RRSanos[[k]][i - 1] &&
  #       RRSanos[[k]][n]/RRSanos[[k]][i - 1] < 1 + U) && 
  #       (1 - U < RRSanos[[k]][n]/RRSanos[[k]][i + 1] &&
  #       RRSanos[[k]][n]/RRSanos[[k]][i + 1] < 1 + U)){
  #       RRSanos[[k]] <- c(RRSanos[[k]], RRSanos[[k]][i])
  #     }    
  #   }
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
      hrvSanos[[k]][i] <- hrvSanos[[k]][i]            
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

setwd("../Infartados/")
for (i in 1:50){
  png(file=paste("Inf-",i,"-hrv_",names(ecgInfartosF[i]),sep=""), width=1800,height=1000)
  plot(hrvInf[[i]], main=paste("No. ",i," ECG ",names(ecgInfartosF[i]),sep=""), type="l")
  dev.off()
}

for (i in 1:50){
  png(file=paste("Inf-",i,"-RR_",names(ecgInfartosF[i]),sep=""), width=1800,height=1000)
  plot(seq(from=0,to=300,by=1/250)[1:75000],
       ecgInfartosF[[i]][1:75000],type="l", , xlab="Tiempo (seg)", ylab="Voltaje (mV)", main=paste("No. ",i," ECG ",names(ecgInfartosF[i]),sep=""))  
  points((seq.int(from=0,to=300,by=1/250))[1:75000][maxInfartos[[i]]$index.max],
         (ecgInfartosF[[i]])[1:75000][maxInfartos[[i]]$index.max],col=2, pch=1)
  dev.off()
}


setwd("../Sanos/")

for (i in 1:50){  
  png(file=paste("San-",i,"-hrv_",names(ecgSanosF[i]),sep=""), width=1800,height=1000)
  plot(hrvSanos[[i]], main=paste("No. ",i," ECG ",names(ecgSanosF[i]),sep=""), type="l")
  dev.off()
}

for (i in 1:50){
  png(file=paste("San-",i,"-RR_",names(ecgSanosF[i]),sep=""), width=1800,height=1000)
  plot(seq(from=0,to=300,by=1/250)[1:75000],
       ecgSanosF[[i]][1:75000],type="l", , xlab="Tiempo (seg)", ylab="Voltaje (mV)", main=paste("No. ",i," ECG ",names(ecgInfartosF[i]),sep=""))  
  points((seq.int(from=0,to=300,by=1/250))[1:75000][maxSanos[[i]]$index.max],
         (ecgSanosF[[i]])[1:75000][maxSanos[[i]]$index.max],col=2, pch=1)  
  dev.off()  
}

setwd("../")