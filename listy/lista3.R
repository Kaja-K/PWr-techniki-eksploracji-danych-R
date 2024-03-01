#1
    library(ggplot2)
    library(class)
    
    data(diamonds)
    new_diamond <- data.frame(carat = 0.24, depth = 55.8, table = 55, price = 322, x = 3.87, y = 4.01, z = 2.86)
    features <- diamonds[, c(1, 5, 6, 7, 8, 9, 10)] # zdefiniowane zmienne
    
    knn(features, new_diamond, cl = diamonds$cut, k = 80) # metoda k-najbliżsyzch sąsiadów z k=80
  
      
#2
    train_indices <- sample(seq_len(nrow(diamonds)), size = 0.7 * nrow(diamonds)) #część ucząca
    train_data <- diamonds[train_indices, ]
    test_data <- diamonds[-train_indices, ] #część testowa 
    
    train_features <- train_data[, c(1, 5, 6, 7, 8, 9, 10)]  # zdefiniowane zmienne
    
    knn(train_features, train_features, train_data$cut, k = 80)# metoda k-najbliżsyzch sąsiadów z k=80
    
    #ocena jakości szlifu dla danego diamentu
    new_diamond <- data.frame(carat = 0.24, depth = 55.8, table = 55, price = 322, x = 3.87, y = 4.01, z = 2.86)
    knn(train_features, new_diamond, train_data$cut, k = 80) 
    
    
#3
    library(carData)
    library(class)
    
    data(Chile)
    
    missing_rows <- which(!complete.cases(Chile$vote))
    
    features <- Chile[, c("age", "income", "statusquo")]
    labels <- Chile$vote
    
    optimal_k <-
      
    imputed_votes <- knn(features[-missing_rows, ], features[missing_rows, ], labels[-missing_rows], k = optimal_k)
    
    Chile$vote[missing_rows] <- imputed_votes