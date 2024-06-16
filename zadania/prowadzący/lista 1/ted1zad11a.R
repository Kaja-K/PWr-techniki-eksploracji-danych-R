library(expsmooth) #w tej bibliotece jest usgdp
length(usgdp)
p=0.7
liczba=round(p*length(usgdp))
us1=window(usgdp, end=c(1988,2))
length(us1)
us2=window(usgdp, start=c(1988,3))
ts.plot(us1, us2, col=c("red", "green"), lty=c(1,2))
legend("topleft", legend=c("zbiór uczący", "zbiór testowy"), col=c("red", "green"), lty=c(1,2))

