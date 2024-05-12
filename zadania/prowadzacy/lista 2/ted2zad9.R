library("ggpubr")
ggscatter(mtcars, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = T, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")
# p is the p-value of Pearson's product-moment correlation
# where the null hypothesis is correlation 0 
# and alternative hypothesis correlation different than 0
cor.test(mtcars$wt, mtcars$mpg, method = "pearson")
plot(mtcars$mpg, mtcars$wt, col="red")
model=lm(mtcars$wt~mtcars$mpg)
summary(model)
abline(model, col="blue", lty=3)
