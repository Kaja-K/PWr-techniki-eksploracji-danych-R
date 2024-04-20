library(ggplot2)
set.seed(240)
dia=data.frame(diamonds)
ran=sample(1:nrow(dia),0.7 * nrow(dia))
nor=function(x) { (x-min(x))/(max(x)-min(x))   }
dia.nor=as.data.frame(lapply(dia[,c(1,5,6,7,8,9,10)], nor))
dia.train=dia.nor[ran,]
dia.test=dia.nor[-ran,]
dia.target=as.factor(dia[ran,2])
test.target=as.factor(dia[-ran,2])
library(class)
accur=function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
k.optm=1
ks=5
ke=50
for (i in ks:ke){
 pr <- knn(dia.train, dia.test, cl=dia.target, k=i)
 tb=table(pr,test.target)
 k.optm[i]=accur(tb)
 cat(i,'=',k.optm[i],'
')}
plot(ks:ke, k.optm[ks:ke], type="b", xlab="k- Value",ylab="Accuracy level")
d=c(24,55.8,55,322,3.87,4.01,2.86)
dia1=dia[ran,c(1,5,6,7,8,9,10)]
dia2=rbind.data.frame(dia1,d)
dia2.norm=as.data.frame(lapply(dia2[,1:7],nor))
d1=dia2.norm[nrow(dia2.norm),]
dia3.norm=dia2.norm[-nrow(dia2.norm),]
dia.target.cat=dia[ran,2]
pr=knn(dia3.norm, d1, cl=dia.target.cat, k=20)
pr
