library(tseries)
ibm=get.hist.quote(instrument="IBM", quote=c("Open", "Close"),
        provider="yahoo", start="2008-01-01", end="2014-01-01") 
                #retclass="ts")
class(ibm)
plot(ibm$Close)
x <- get.hist.quote(instrument = "^gspc", start = "1998-01-01",
                      quote = "Close")
plot(x)  
