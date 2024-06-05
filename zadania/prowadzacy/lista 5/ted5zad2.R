library(rpart)
library(rpart.plot)
library(party)
library(caTools)
set.seed(123)
sample=sample.split(readingSkills[,1:4], SplitRatio=0.85)
reading.train=subset(readingSkills[sample==T,])
reading.test=subset(readingSkills[sample==F,])
mojedrzewo=rpart(nativeSpeaker~age+score, 
                 data=reading.train, method="class",
                 parms=list(split="gini"))
rpart.plot(mojedrzewo)
prp(mojedrzewo,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output
accur=function(x){sum(diag(x))/sum(rowSums(x))*100}
prognozy=predict(mojedrzewo, newdata=reading.test, type="class")
a=table(prognozy, reading.test$nativeSpeaker)
a
accur(a)
błąd.testowy=100-accur(a)
błąd.testowy

prognozy=predict(mojedrzewo, newdata=reading.train, type="class")
a=table(prognozy, reading.train$nativeSpeaker)
a
accur(a)
błąd.treningowy=100-accur(a)
błąd.treningowy


mojedane=data.frame(age=c(11,7), 
                    shoeSize=c(NA, NA), 
                    score=c(34.45628, 36.34538))
predict(mojedrzewo, newdata=mojedane, type="class")



