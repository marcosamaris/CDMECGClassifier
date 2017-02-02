#library("EbayesThresh")

ywdEBT <- ebayesthresh.wavelet(ywd)

ywrEB <- wr(ywdEBT)

x <- (1:1024)/1024
plot(x, ywrEB, type="l", xlab="x",
ylab="EBayesThresh Estimate")
lines(x, v$bumps, lty=2)

FineWSTCoefs <- accessD(ywst, lev=nlevels(ywd)-1)
sigmaWST <- mad(FineWSTCoefs)
utWSTDJ <- sigmaWST*sqrt(2*log(1024))

ywstT <- threshold(ywst, policy="manual", value=utWSTDJ)

plot(ywstT, scaling="by.level", main="", sub="")