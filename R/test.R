obs = 1000
inSample = 100
holdout = obs - inSample

library(forecast)
ourData = rnorm(obs, 100, 20)

L = 1
fcs = array(0, holdout)
for(i in (inSample+1):(obs)){
  fcs[i-inSample] = sum(meanf(ourData[1:i], h=L)$mean)
}
fcs

outp(data=ourData, fcs=fcs, holdout=holdout, ss="dynamic", L=L, CSL=95)
outp(data=ourData, fcs=fcs, holdout=holdout, ss="constant", L=L, CSL=95)
outp(data=ourData, fcs=fcs, holdout=holdout, ss=qnorm(0.5,0,20), L=L, CSL=0.5)

x <- c(6,3,4,4,5,7,4,3,4,5,6,6,7,8,3)
ftd <- 5
fcs <- rep(10,8)
outp(data=x, fcs=fcs, fitted=ftd, holdout=8, ss=3.62845894088858, L=2, CSL=0.95)


test <- outp(data=ourData, fcs=fcs, holdout=holdout, ss=qnorm(0.5,0,20), L=L, CSL=0.5)
plot(ourData[(inSample+1):obs])
points(test$ord[-(1:L)],col="red")
points(test$inv[-(1:L)],col="blue")

