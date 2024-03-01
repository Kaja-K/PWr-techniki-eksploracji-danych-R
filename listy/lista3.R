# install.packages("carData")
# install.packages("caret")

library(carData)
library(class)
library(caret)
library(ggplot2)
library(class)


#1
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
    data(Chile)
    
    data <- Chile[, c(4, 6, 7, 8)] #wybranie kolumn
    filter_data <- data[which(complete.cases(data[, c("age", "income", "statusquo")])), ]# wyfiltrowanie kompletnych danych
    data_na <- filter_data[which(is.na.data.frame(filter_data[, "vote"])), ] # odfiltrowanie NA
    
    cases <- data[which(complete.cases(data)), ] # wiersze z kompletnymi danymi
    
    nor <- function(x) { (x - min(x)) / (max(x) - min(x)) } # normalizacja danych
    cases_normalized <- as.data.frame(lapply(cases[, c("age", "income", "statusquo")], nor)) #normalizacja kolumn
    
    
    # Funkcja do imputacji brakujących wartości za pomocą kNN
    impute_vote_knn <- function(data, k) {
      train_data <- data[complete.cases(data), c("age", "income", "statusquo")]
      predict_data <- data[which(is.na(data$vote)), c("age", "income", "statusquo")]
      if (any(is.na(predict_data))) {predict_data <- 
        apply(predict_data, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))}
      
      knn_model <- knn(train_data, predict_data, data$vote[complete.cases(data)], k = k)
      return(knn_model)}
    
    ctrl <- trainControl(method = "cv", number = 5)  # walidacja krzyżowa
    
    knn_model <- train(x = cases_normalized, y = cases$vote,method = "knn",trControl = ctrl,
                       tuneGrid = data.frame(k = seq(1, 10))) # trenowanie modelu
    
    optimal_k <- knn_model$bestTune$k #optymalna wartość k
    imputed_votes <- impute_vote_knn(data, optimal_k) # imputowanie brakujących wartości
    data$vote[which(is.na(data$vote))] <- imputed_votes # zastąpienie brakujących wartości imputowanymi
    
    levels(data$vote) #nie ma tu wartości NA
    