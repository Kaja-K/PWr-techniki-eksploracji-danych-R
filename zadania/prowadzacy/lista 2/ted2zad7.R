library(ggplot2)
out<-boxplot.stats(mpg$hwy)$out
out_ind <- which(mpg$hwy %in% c(out))
out
out_ind
mpg[out_ind,]
boxplot(mpg$hwy,
        ylab = "hwy",
        main = "Boxplot of highway miles per gallon"
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
m=mean(mpg$hwy)
sigma=sd(mpg$hwy)
upper=m+3*sigma
lower=m-3*sigma
outlier_ind <- which(mpg$hwy < lower | mpg$hwy > upper)
outlier_ind
mpg[outlier_ind,]
mpg[outlier_ind, "hwy"]


