library(lattice)
xyplot(Nile, cut=list(number=3, overlap=0.5))
xyplot(Nile, strip=FALSE, cut=list(number=3, overlap=0.5))
