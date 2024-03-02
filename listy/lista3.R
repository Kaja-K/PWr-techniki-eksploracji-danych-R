#install.packages(c("carData","caret"))

library(carData)
library(class)
library(caret)
library(ggplot2)
library(class)

# Powtarzalność podziałów
set.seed(123) 

# Dane
data(diamonds) # 1,2 zadanie
data(Chile) # 3 zadanie


# 1. Klasyfikacja dla nowego diamentu

    # Stworzenie ramki danych dla nowego diamentu
    new_diamond <- data.frame(carat = 0.24, depth = 55.8, table = 55, price = 322, x = 3.87, y = 4.01, z = 2.86)
    
    # Wybór zmiennych do klasyfikacji
    features <- diamonds[, c(1, 5, 6, 7, 8, 9, 10)]
    
    # Użycie metody k-najbliższych sąsiadów dla klasyfikacji nowego diamentu
    knn(features, new_diamond, cl = diamonds$cut, k = 80)
    
      
# 2. Podział na część uczącą i testową
    
    # Losowy podział danych na część uczącą i testową
    train_indices <- sample(seq_len(nrow(diamonds)), size = 0.7 * nrow(diamonds))
    train_data <- diamonds[train_indices, ]
    test_data <- diamonds[-train_indices, ]
    
    # Wybór zmiennych do klasyfikacji
    train_features <- train_data[, c(1, 5, 6, 7, 8, 9, 10)]
    
    # Użycie metody k-najbliższych sąsiadów dla klasyfikacji na danych uczących i dla nowego diamentu
    knn(train_features, train_features, train_data$cut, k = 80)
    knn(train_features, new_diamond, train_data$cut, k = 80)
    
    
# 3. Imputacja brakujących wartości dla danych Chile
    
    # Wybór zmiennych do analizy
    data <- Chile[, c(4, 6, 7, 8)]
    
    # Filtracja kompletnych danych i wyfiltrowanie brakujących
    filter_data <- data[which(complete.cases(data[, c("age", "income", "statusquo")])), ]
    data_na <- filter_data[which(is.na.data.frame(filter_data[, "vote"])), ]
    
    # Wiersze z kompletnymi danymi
    cases <- data[which(complete.cases(data)), ]
    
    # Normalizacja danych
    nor <- function(x) { (x - min(x)) / (max(x) - min(x)) }
    cases_normalized <- as.data.frame(lapply(cases[, c("age", "income", "statusquo")], nor))
    
    # Funkcja do imputacji brakujących wartości za pomocą kNN
    impute_vote_knn <- function(data, k) {
      train_data <- data[complete.cases(data), c("age", "income", "statusquo")]
      predict_data <- data[which(is.na(data$vote)), c("age", "income", "statusquo")]
      if (any(is.na(predict_data))) {predict_data <- apply(predict_data, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))}
      knn_model <- knn(train_data, predict_data, data$vote[complete.cases(data)], k = k)
      return(knn_model)}
    
    # Walidacja krzyżowa
    ctrl <- trainControl(method = "cv", number = 5)
    
    # Trenowanie modelu k-najbliższych sąsiadów
    knn_model <- train(x = cases_normalized, y = cases$vote, method = "knn", trControl = ctrl, tuneGrid = data.frame(k = seq(1, 10)))
    
    # Optymalna wartość k
    optimal_k <- knn_model$bestTune$k
    
    # Imputacja brakujących wartości
    imputed_votes <- impute_vote_knn(data, optimal_k)
    data$vote[which(is.na(data$vote))] <- imputed_votes
    
    # Wyświetlenie unikalnych wartości w zmiennej 'vote' po imputacji
    levels(data$vote)
    