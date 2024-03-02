#install.packages(c("cluster","factoextra"))

library(cluster)
library(factoextra)

# Powtarzalność podziałów
set.seed(123) 

# Dane
data(iris) # 1 zadanie
data(diamonds) # 2 zadanie
data(votes.repub) # 3 zadnaie
data(pluton) # 4 zadanie


# 1. Klasteryzacja K-średnich na zbiorze danych Iris
      
    # Wybór atrybutów
    iris_numeric <- iris[, 1:4] 
    
    # Grupowanie k-średnich
    kmeans_result <- kmeans(iris_numeric, centers = 3, nstart = 20) 
    
    # Macierz pomyłek
    table(iris$Species, kmeans_result$cluster)
    
    # Wizualizacja klastrów
    fviz_cluster(kmeans_result, data = iris_numeric, geom = c("point", "text"), stand = FALSE, 
                 ellipse.type = "convex", main = "K-means Clustering of Iris Data", ggtheme = theme_bw())
    
    
# 2. Klasteryzacja K-średnich na zbiorze danych Diamonds

    diamonds_sample <- diamonds[sample(nrow(diamonds), 1000), c("carat", "depth", "table", "price")]
    
    # Metoda "łokcia" dla wyboru optymalnej liczby klastrów
    for (i in 1:10) {kmeans_model <- kmeans(diamonds_sample, centers = i, nstart = 20) wss[i] <- sum(kmeans_model$withinss)}
    
    # Wykres "łokcia" dla oceny optymalnej liczby klastrów
    plot(1:10, wss, type = "b", pch = 19, frame = FALSE, main = "Elbow Method for Optimal k")
    
    # Klasteryzacja K-średnich (optymalna liczba klastrów to 3)
    kmeans_result <- kmeans(diamonds_sample, centers = 3, nstart = 20) 
    
    # Wizualizacja klastrów
    fviz_cluster(kmeans_result, geom = c("point", "text"), data = diamonds_sample[, c("carat", "price")], 
                 stand = FALSE, ellipse.type = "convex", main = "K-Means Clustering", ggtheme = theme_bw())
    
    
# 3. Klasteryzacja K-średnich na zbiorze danych votes.repub
    
    # Usunięcie wierszy zawierających NA
    votes.repub_numeric <- sapply(votes.repub[, -1], as.numeric)
    votes.repub_numeric <- na.omit(votes.repub_numeric) 
    
    # Metoda "łokcia" dla wyboru optymalnej liczby klastrów
    wss <- numeric(10)
    for (i in 1:10) {kmeans_model <- kmeans(votes.repub_numeric, centers = i, nstart = 20) 
                           wss[i] <- sum(kmeans_model$withinss)}
    
    # Wykres "łokcia" dla oceny optymalnej liczby klastrów
    plot(wss, type = "b", main = "Elbow Method for Optimal k")
    
    # Klasteryzacja K-średnich (optymalna liczba klastrów to 3)
    kmeans_result <- kmeans(votes.repub_numeric, centers = 3, nstart = 20) 
    
    # Wizualizacja klastrów
    fviz_cluster(kmeans_result, data = votes.repub_numeric, stand = FALSE, ellipse.type = "convex", 
                 main = "K-Means Clustering", ggtheme = theme_bw())

        
# 4. Klasteryzacja K-średnich na zbiorze danych pluton
    
    # Metoda "łokcia" dla wyboru optymalnej liczby klastrów
    wss <- numeric(10)
    for (i in 1:10) {kmeans_model <- kmeans(pluton[, 1:ncol(pluton)], centers = i, nstart = 20)
                           wss[i] <- sum(kmeans_model$withinss)}
    
    # Wykres "łokcia" dla oceny optymalnej liczby klastrów
    plot(wss, type = "b", main = "Elbow Method for Optimal k")
    
    # Klasteryzacja K-średnich (optymalna liczba klastrów to 3)
    kmeans_result <- kmeans(pluton[, 1:ncol(pluton)], centers = 3, nstart = 20) 
    
    # Wizualizacja klastrów
    fviz_cluster(kmeans_result, data = pluton[, 1:ncol(pluton)], stand = FALSE,
                 ellipse.type = "convex", main = "K-Means Clustering", ggtheme = theme_bw())
    