library(class)
library(ggplot2)
colnames(diamonds)
dia=as.data.frame(diamonds)
class(dia[,"cut"])
levels(dia[,"cut"])
dia1=dia[,c(1,5,6,7,8,9,10)]
d=c(24,55.8,55,322,3.87,4.01, 2.86)
dia2=rbind(dia1,d)
nor=function(x) { (x-min(x))/(max(x)-min(x))   }
dia2.norm=as.data.frame(lapply(dia2[,1:7], nor))
d1=dia2.norm[nrow(dia2.norm),]
dia3.norm=dia2.norm[-nrow(dia2.norm),]
dia.target=dia[,2]
pr=knn(dia3.norm, d1, cl=dia.target, k=80)
pr
