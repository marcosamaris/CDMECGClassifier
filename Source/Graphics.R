VisualizeRR <- function() {  
  #dev.off()
  i <- sample(1:length(ecgF), 1)
  ## visualize the result
  for (i in 1:len ) {    
  plot(seq(from=0,to=60,by=1/360)[1:21600],
       ecgF[[i]][1:21600],type="l", , xlab="Tiempo (seg)", ylab="Voltaje (mV)", main=paste("ECG ",names(ecgF[i]),sep=""))
  
  points((seq.int(from=0,to=60,by=1/360))[1:21600][maxInfartos[[i]]$index.max],
         (ecgF[[i]])[1:21600][maxInfartos[[i]]$index.max],col=2, pch=1)
  }
}

VisualizeHRV <- function() {
  dev.off()
  i <- sample(1:length(ecgInfartosF), 1)
  j <- sample(1:length(ecgSanosF), 1)
  par(mfrow=c(1,2))    
  plot.ts(hrvInf[[i]], xlab="No. de latido", ylab="HRV (Latidos por seg)", main="HRV de Ecg Infartado")
  plot.ts(hrvSanos[[j]], xlab="No. de latido", ylab="HRV (Latidos por seg)", main="HRV de Ecg Sano")
}


##
## Tiempo total de los algoritmos de la aplicación
##
Time <- c(4.894,21.381,83.514,214.945,433.087,860.878,1258.695)
plot(Time,type="l",axes=FALSE, ann=FALSE)
plot(Time, type="o",axes=FALSE, ann=FALSE)
axis(1, at=1:7, lab=c("1","5","20","50", "100", "200", "300"))
axis(2, at=0:max(Time))
title(main="Tiempo total de ejecución de los Algoritmos\n según número de señales", font.main=4)
# Label the x and y axes with dark green text
title(xlab="Número de señales")
title(ylab="Tiempo")
box()