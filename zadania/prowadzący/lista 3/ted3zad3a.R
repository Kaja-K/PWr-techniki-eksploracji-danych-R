library(carData)
class(Chile)
dane=Chile[,c(4,6,7,8)]
danecz1=dane[which(complete.cases(dane[,1:3])),]#wyrzuca wiersza w których jest NA w kolumach 1,2,3
danena=danecz1[which(is.na.data.frame(danecz1[,"vote"])),]#dane z NA w kolumnie vote
nrow(danena)
danecz=dane[which(complete.cases(dane)),]# dane z kompletnymi wierszami
nor=function(x) { (x -min(x))/(max(x)-min(x))   }
library(class)
set.seed(1)
#podział na zbiór uczący i testowy aby znaleźć optymalne k
ran=sample(1:nrow(danecz),0.7 * nrow(danecz))
dane.nor=as.data.frame(lapply(danecz[,c(1,2,3)], nor))
dane.train=dane.nor[ran,]
dane.test=dane.nor[-ran,]
dane.target=as.factor(danecz[ran,4])
test.target=as.factor(danecz[-ran,4])
accur=function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accurk=numeric(100)
for (i in 1:100){
  pr <- knn(dane.train, dane.test, cl=dane.target, k=i)
  tb=table(pr,test.target)
  accurk[i]=accur(tb)
  cat(i,'=',accurk[i],'
')}
plot(accurk, type="b", xlab="k- Value",ylab="Accuracy level")
which.max(accurk)
danecale=rbind.data.frame(danecz[,1:3], danena[,1:3])#łączymy dane kompletne i dane z vote NA bez kolumny vote
danecale.norm=as.data.frame(lapply(danecale,nor))#normujemy
dane.train=danecale[1:nrow(danecz),]#dane do uczenia
dane.uzup=danecale[-(1:nrow(danecz)),]#dane do uzupełnienia, klasyfikacji kolumny vote
cl.target=danecz[,4]#zadane wartości vote w zbiorze uczącym
pr=knn(dane.train, dane.uzup, cl=cl.target, k=44)
pr #uzupełnienie, predykacja
dane.uzup=cbind.data.frame(danena[,1:3],vote=pr)#uzupełnienie vote w NA
danebezna=rbind(danecz,dane.uzup)#całe dane z uzupełnionymi vote
