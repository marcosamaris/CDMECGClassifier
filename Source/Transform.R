Normalizar <- function() {
  ZInf <<- list()
  ZSanos <<- list()
  for (i in names(hrvSanos)){    
    ZSanos[[i]] <<- znorm(t(hrvSanos[[i]][1:(length(hrvSanos[[i]]))]))
  }  
  for (i in names(hrvInf)){    
    ZInf[[i]] <<- znorm(t(hrvInf[[i]][1:(length(hrvInf[[i]]))]))
  }
}


SAX <- function() {
  ZSAXInf <<- list()
  ZSAXSanos <<- list()
  for (i in names(ZInf)){    
    ZSAXInf[[i]] <<- ts2string(ZInf[[i]],5)
  }
  for (i in names(ZSanos)){    
    ZSAXSanos[[i]] <<- ts2string(ZSanos[[i]],5)
  }
}

CDMMatrix <- function() {  
  Temp <- c(ZSAXInf[1:10],ZSAXSanos[1:10])
  N <- length(Temp)
  CDMMat <<- matrix(nrow=N,ncol=N)
  for (i in 1:N){
    write(Temp[[i]], file="TempA.txt",ncolumns=1,sep="")
    system("gzip -f TempA.txt")
    for (j in 1:N){
      write(Temp[[j]], file="TempB.txt",ncolumns=1,sep="")
      write(c(Temp[[j]], Temp[[i]]), file="TempAB.txt",ncolumns=1,sep="")
      system("gzip -f TempB.txt")
      system("gzip -f TempAB.txt")
      CDMMat[i, j] <<- file.info("TempAB.txt.gz")$size/
        (file.info("TempA.txt.gz")$size+
        file.info("TempB.txt.gz")$size)
    }
  }
}
