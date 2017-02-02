# ecgInfartosF <<- matrix(0, nrow=75000, ncol=len);
# ecgInfartosF[,i] <<- wavShrink(ecgInfartos[,i], wavelet="d2",shrink.fun="soft", thresh.fun="universal", thresh.scale=.8, xform="modwt")
#
#
# #           ## calculate the CWT
# #
# #           ## form CWT tree 
# #           z <- wavCWTTree(W)
# #           
# #           ## estimate the peak locations using default
# #           ## scale.range 
# #           peak <- wavCWTPeaks(z)
# #           
# #           ## plot an overlay of the original series and the
# #           ## peaks
# #           plot(ts(ecgInfartosF[,i][1:1024],frequency=250), type="l", xlab="time", ylab=sampleName)
# #           points(peak, pch=16, col="red", cex=1.2)
# 
# ecgSanosF <<- matrix(0, nrow=75000, ncol=len);
#           ecgSanosF[,i] <<- wavShrink(ecgSanos[,i], wavelet="d2",shrink.fun="soft", thresh.fun="universal", thresh.scale=.8, xform="modwt")                          
#           
# #           ## calculate the CWT 
# #           W <- wavCWT(ecgSanosF[,i])
# #           
# #           ## form CWT tree 
# #           z <- wavCWTTree(W)
# #           
# 
# #           ## estimate the peak locations using default 
# #           ## scale.range 
# #           peak <- wavCWTPeaks(z)
# #           
# #           ## plot an overlay of the original series and the 
# #           ## peaks 
# #           
# #           plot(ecgSanosF[,i], type="l", xlab="time", ylab=sampleName)
# #           points(peak, pch=16, col="red", cex=1.2)
 
library(wavethresh)
v <- DJ.EX()
x <- (1:1024)/1024
plot(x, v$bumps, type="l", ylab="Bumps")

ssig <- sd(v$bumps) # Bumps sd
SNR <- 2 # Fix our SNR
sigma <- ssig/SNR

e <- rnorm(1024, mean=0, sd=sigma)

y <- v$bumps + e
plot(x, y, type="l", ylab="Noisy bumps")

#Plot wd of bumps
xlv <- seq(from=0, to=1.0, by=0.2)
bumpswd <- wd(v$bumps)
plot(bumpswd, main="", sub="",xlabvals=xlv*512, xlabchars=as.character(xlv), xlab="x")

#Plot wd of noisy bumps for comparison
ywd <- wd(y)
plot(ywd, main="", sub="", xlabvals=xlv*512, xlabchars=as.character(xlv),xlab="x")

FineCoefs <- accessD(ywd, lev=nlevels(ywd)-1)
sigma <- mad(FineCoefs)
utDJ <- sigma*sqrt(2*log(1024))

ywdT <- threshold(ywd, policy="manual", value=utDJ)

ywr <- wr(ywdT)
plot(x, ywr, type="l")
lines(x, v$bumps, lty=2)

ywdcvT <- threshold(ywd, policy="cv", dev=madmad)

ywrcv <- wr(ywdcvT)

plot(x, ywrcv, type="l", xlab="x", ylab="Cross-val.Estimate")
lines(x, v$bumps, lty=2)