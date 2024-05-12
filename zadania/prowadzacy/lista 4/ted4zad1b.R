library(ClusterR)
library(cluster)
iris_1=iris[,-5]#wyrzuca kolumnę Species
iris_2=scale(iris_1)#skaluje kolumnami
set.seed(245)
kmeans.re <- kmeans(iris_2, centers = 3, iter.max=20, nstart = 30)# grupuje w 3 grupy
kmeans.re #wyświetla podstawowe charakterystki grupowania
kmeans.re$cluster #wyświetla numery grup
table(iris$Species)#wyświetla ile jest wartości w zmiennej Species
cm <- table(iris$Species, kmeans.re$cluster)#tworzy macierz pomyłek bez uporządkowania - trzeba zobaczyć gdzie najczęściej
cm
spec=as.factor(ifelse(kmeans.re$cluster==2, "setosa", ifelse(kmeans.re$cluster==3, "versicolor", "virginica")))#porządkuje do najczęstych
cm.kor=table(iris$Species,spec)
cm.kor
plot(iris_1[,c("Sepal.Length", "Sepal.Width")]) #rysuje punkty (Sepal.Length, Sepal.Width)
plot(iris_1[,c("Sepal.Length", "Sepal.Width")], 
     col = kmeans.re$cluster, 
     main = "K-means with 3 clusters")# to samo co wyżej tylko oznacza je kolorem z zależności od Species
centers1=kmeans.re$centers[,"Sepal.Length"]*sd(iris_1[,"Sepal.Length"])+mean(iris_1[,"Sepal.Length"])
centers2=kmeans.re$centers[,"Sepal.Width"]*sd(iris_1[,"Sepal.Width"])+mean(iris_1[,"Sepal.Width"])
center=cbind(centers1,centers2)
# cex is font size, pch is symbol
points(center, 
       col = 1:3, pch = 8, cex = 3) 
y_kmeans <- kmeans.re$cluster
clusplot(iris_1[, c("Sepal.Length", "Sepal.Width")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster iris"),
         xlab = 'Sepal.Length',
         ylab = 'Sepal.Width')
library(factoextra)
fviz_cluster(kmeans.re, data = iris_1, 
    palette = c("blue", "red","green"), 
geom =c("point", "text"), 
ellipse.type = "convex",ggtheme = theme_bw())
cbind.data.frame(iris[,5], kmeans.re$cluster)

