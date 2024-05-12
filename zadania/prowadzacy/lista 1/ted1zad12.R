library(quantmod)
# https://finance.yahoo.com/
djia=getSymbols(Symbols="DJIA", src="yahoo", 
               from=Sys.Date()-365, to=Sys.Date(), auto.assign=F)
djia.close=djia$DJIA.Close
plot(djia.close)
head(djia)
djia[1:4,]
ibm=getSymbols(Symbols="IBM", src="yahoo", 
               from="2005-01-01", to="2010-12-31", auto.assign=F, return.class="ts")
head(ibm)
ibm.close=ibm[,"IBM.Close"]
plot(ibm.close)
