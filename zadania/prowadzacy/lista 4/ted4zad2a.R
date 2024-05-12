library(cluster)
library(factoextra)
levels(diamonds$cut)
set.seed(1)
dane=diamonds
dane$cut=as.numeric(ifelse(dane$cut=="Ideal", 1,ifelse(dane$cut=="Premium", 2, ifelse(dane$cut=="Very Good", 3, ifelse(dane$cut=="Good", 4, 5)))))
rows=sample(nrow(dane), 1000)
dane=dane[rows,c(1,2,5,6,7,8,9)]
dane=na.omit(dane)
danes=scale(dane)
fviz_nbclust(danes, kmeans, method = "wss", k.max=12)+
geom_vline(xintercept =5 , linetype=2)+
labs(subtitle = "Metoda=łokcia")
km.diamonds=kmeans(danes, 5, iter.max = 20, nstart = 25)
fviz_cluster(km.diamonds, data = dane, 
  palette = c("blue", "red","green","grey","brown","orange"),
  geom =c("point", "text"), 
  ellipse.type = "convex",ggtheme = theme_bw())
y_kmeans=km.diamonds$cluster
clusplot(dane[, c(1,2)],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster diamonds"),
         xlab = 'Carat',
         ylab = 'cut')
dane=diamonds[,c(1,5,6,7,8,9)]
dane=na.omit(dane)
danes=scale(dane)
km.diamonds=kmeans(danes, 4, iter.max = 20, nstart = 25)
fviz_cluster(km.diamonds, data = dane, palette = c("blue", "red","green","grey","brown"), geom =c("point", "text"), ellipse.type = "convex",ggtheme = theme_bw())