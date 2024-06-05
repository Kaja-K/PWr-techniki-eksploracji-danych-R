library(rpart)
library(rpart.plot)
mydata=read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
mydata=as.data.frame(mydata)
mydata$admit=as.factor(mydata$admit)
mydata$rank=factor(mydata$admit, ordered=T, levels=c(1,2,3,4))
set.seed(12)
indeks=sample(400, 350)
admit.train=mydata[indeks, ]
admit.test=mydata[-indeks, ]
mojedrzewo=rpart(admit~., data=admit.train, method="class",
                 parms=list(split="information"))
rpart.plot(mojedrzewo)
prp(mojedrzewo,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output
accur=function(x){sum(diag(x))/sum(rowSums(x))*100}
prognozy=predict(mojedrzewo, newdata=admit.train, type="class")
a=table(prognozy, admit.train$admit)
a
accur(a)
prognozy=predict(mojedrzewo, newdata=admit.test, type="class")
a=table(prognozy, admit.test$admit)
a
accur(a)
mojedane=data.frame(gre=as.integer(c(700,650)), gpa=c(3.7,4), rank=factor(c(2,1), ordered = T))
predict(mojedrzewo, newdata=mojedane, type="class")
