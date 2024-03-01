#install.packages("cluster")
#install.packages("factoextra")

library(cluster)
library(factoextra)

#1
    data(iris)
    
    iris_numeric <- iris[, 1:4] # Wybór atrybutów
    kmeans_result <- kmeans(iris_numeric, centers = 3, nstart = 20) # Grupowanie k-średnich
    
    table(iris$Species, kmeans_result$cluster)# Macierz pomyłek
    
    fviz_cluster(kmeans_result, data = iris_numeric, geom = c("point", "text"), stand = FALSE, ellipse.type = "convex",
                 main = "K-means Clustering of Iris Data",ggtheme = theme_bw())
    
    
#2
    data(diamonds)
    
    diamonds_sample <- diamonds[sample(nrow(diamonds), 1000), c("carat", "depth", "table", "price")] # 1000 wierszy
    
    for (i in 1:10) { kmeans_model <- kmeans(diamonds_sample, centers = i, nstart = 20) 
                      wss[i] <- sum(kmeans_model$withinss)} # Metoda "łokcia"
    
    plot(1:10, wss, type = "b", pch = 19, frame = FALSE, main = "Elbow Method for Optimal k")
    
    kmeans_result <- kmeans(diamonds_sample, centers = 3, nstart = 20) #Grupowanie k-średnich (3 - odczytane "łokcia" z wykresu)
    

    fviz_cluster(kmeans_result, geom = c("point", "text"), data = diamonds_sample[, c("carat", "price")], 
                 stand = FALSE, ellipse.type = "convex", main = "K-Means Clustering",ggtheme = theme_bw())
    
    
#3
    data(votes.repub)
    votes.repub_numeric <- sapply(votes.repub[, -1], as.numeric)
    
    votes.repub_numeric <- na.omit(votes.repub_numeric)# usunięcie wierszy zawierających NA
    
    wss <- numeric(10)
    for (i in 1:10) {kmeans_model <- kmeans(votes.repub_numeric, centers = i, nstart = 20)
                     wss[i] <- sum(kmeans_model$withinss)}  # Metoda "łokcia" 

    plot(1:10, wss, type = "b", pch = 19, frame = FALSE, main = "Elbow Method for Optimal k")
    
    kmeans_result <- kmeans(votes.repub_numeric, centers = 3, nstart = 20) #Grupowanie k-średnich (3 - odczytane "łokcia" z wykresu)
    
    fviz_cluster(kmeans_result, data = votes.repub_numeric, stand = FALSE, 
                 ellipse.type = "convex", main = "K-Means Clustering",ggtheme = theme_bw())
    
    
#4
    data(pluton)
    
    wss <- numeric(10)
    for (i in 1:10) {kmeans_model <- kmeans(pluton[, 1:ncol(pluton)], centers = i, nstart = 20)
      wss[i] <- sum(kmeans_model$withinss)}# Metoda "łokcia" 
    
    plot(1:10, wss, type = "b", pch = 19, frame = FALSE, main = "Elbow Method for Optimal k")
    
    kmeans_result <- kmeans(pluton[, 1:ncol(pluton)], centers = 3, nstart = 20) #Grupowanie k-średnich (3 - odczytane "łokcia" z wykresu)
    
    fviz_cluster(kmeans_result, data = pluton[, 1:ncol(pluton)], stand = FALSE,
                 ellipse.type = "convex", main = "K-Means Clustering",ggtheme = theme_bw())