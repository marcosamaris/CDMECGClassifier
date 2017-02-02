
CargarECGInfartados <- function(projectDir=".") {    
  setwd("./ECG-GIIB/")
  txtfiles <- list.files(paste(projectDir),'.txt$');        
  len <- length(txtfiles);
  ecgTemp <- matrix(0, nrow=75000, ncol=len);
  Names <- vector()        
  
  for (i in 1:len ) {    
    filename <- paste(projectDir, "/" , txtfiles[i],sep='');                
    sample <- read.table(filename, nrow=75000);
    if(nrow(sample) == 0) {
      cat("El archivo ", filename, " se encuentra vacio\n");
      next;
    }
    sampleName <- filename; sampleName=gsub("./", "",sampleName); sampleName=gsub(".txt", "", sampleName);
    ecgTemp[,i] <- sample$V1
    Names[i] <- sampleName
    cat("Importar señal Infartada No.",i, "Archivo", sampleName, "\n");
  }
  ecgInfartos <<- as.list(as.data.frame(ecgTemp))
  names(ecgInfartos) <<- Names
  setwd("../")
}


CargarECGSanos <- function(projectDir=".") {
setwd("./ECG-FCV/")
txtfiles <- list.files(paste(projectDir),'.txt$');
len = length(txtfiles);
ecgTemp <- matrix(0, nrow=75000, ncol=len);
Names <- vector()

for (i in 1:len ) {
  filename = paste(projectDir, "/" , txtfiles[i],sep='');                
  sample = read.table(filename, nrow=75000);  
  if(nrow(sample) == 0) {
    cat("El archivo ", filename, " se encuentra vacío\n");
    next;
  }
  sampleName <- filename; sampleName=gsub("./", "",sampleName); sampleName=gsub(".txt", "", sampleName);
  Names[i] <- sampleName
  ecgTemp[,i] <- sample$V1;  
  cat("Importar señal Sana No.",i, "Archivo", sampleName, "\n");
}
ecgSanos <<- as.list(as.data.frame(ecgTemp))
names(ecgSanos) <<- Names
setwd("../")
}


#CargarRDatada() <- function(){
#  load("./RData/ECGInfartos.RData")
#  load("./RData/ECGSanos.RData")
#}