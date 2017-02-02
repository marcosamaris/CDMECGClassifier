#rm(list=ls())
setwd('~/Dropbox/Doctorade/Articles/First/App/mitdb/');

load("./RData/HRV-todos.RData")

library("msProcess")

library("cluster")
library("svWidgets")
library("rpanel")

source("./Source/Cargar_DB.R")
source("./Source/PreProc.R")
source("./Source/PAA_SAX.R")
source("./Source/Transform.R")
source("./Source/Clasific.R")
source("./Source/Graphics.R")
source("./Source/Rpanel-Visual.R")

#library(relimp, pos=4)

