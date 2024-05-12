#install.packages(c("party", "rpart", "rpart.plot", "caTools", "ISLR", "DAAG", "modeldata"))

library(rpart)
library(party)
library(rpart.plot)
library(caTools)
library(ISLR)
library(DAAG)
library(modeldata)

# Powtarzalność podziałów
set.seed(123) 

# Dane
data(readingSkills) # 1,2 zadanie
mydata <- read.csv("listy/dane/binary.csv") #3 zadanie
data("Carseats") # 4 zadanie
data(spam7) # 5 zadanie
data(ames) # 6 zadanie


#1. Podział danych i budowa drzewa decyzyjnego z kryterium zysku informacyjnego
  
    # Podział danych na zbiór treningowy i testowy
    indices <- sample(1:nrow(readingSkills), 0.7 * nrow(readingSkills))
    train_data <- readingSkills[indices, ]
    test_data <- readingSkills[-indices, ]
    
    # Budowa drzewa decyzyjnego
    tree_info <- rpart(nativeSpeaker ~ age + score, data = train_data, parms = list(split = "information"))
    
    # Wizualizacja drzewa
    rpart.plot(tree_info)
    
    # Błąd treningowy
    train_pred_info <- predict(tree_info, newdata = train_data, type = "class")
    sum(train_data$nativeSpeaker != train_pred_info) / nrow(train_data)
    
    # Błąd testowy
    test_pred_info <- predict(tree_info, newdata = test_data, type = "class")
    sum(test_data$nativeSpeaker != test_pred_info) / nrow(test_data)
    
    # Prognoza dla nowych danych
    new_data <- data.frame(age = c(11, 7), score = c(34.45628, 36.34538))
    predict(tree_info, newdata = new_data, type = "class")
    
    
# 2. Budowa drzewa decyzyjnego z kryterium Gini'ego
    
    split <- sample.split(readingSkills$nativeSpeaker, SplitRatio = 0.7)
    train_data <- subset(readingSkills, split == TRUE)
    test_data <- subset(readingSkills, split == FALSE)
    
    # Budowa drzewa decyzyjnego
    tree_gini <- rpart(nativeSpeaker ~ age + score, data = train_data)
    
    # Wizualizacja drzewa
    rpart.plot(tree_gini)
    
    # Błąd treningowy
    train_pred_gini <- predict(tree_gini, newdata = train_data, type = "class")
    sum(train_data$nativeSpeaker != train_pred_gini) / nrow(train_data)
    
    # Błąd testowy
    test_pred_gini <- predict(tree_gini, newdata = test_data, type = "class")
    sum(test_data$nativeSpeaker != test_pred_gini) / nrow(test_data)
    
    # Prognoza dla nowych danych
    new_data <- data.frame(age = c(11, 7), score = c(34.45628, 36.34538))
    predict(tree_gini, newdata = new_data, type = "class")
    

# 3. Budowa drzewa klasyfikacyjnego dla danych binary.csv  
    
    split <- sample.split(mydata$admit, SplitRatio = 0.7)
    train_data <- subset(mydata, split == TRUE)
    test_data <- subset(mydata, split == FALSE)
    
    # Zmiana zmiennej na factor
    train_data$admit <- factor(train_data$admit)
    
    # Budowa drzewa klasyfikacyjnego
    tree_info <- rpart(admit ~ gre + gpa + rank, data = train_data)
    
    # Podsumowanie i wizualizacja drzewa
    summary(tree_info)
    plot(tree_info)
    text(tree_info)
    
    # Trafność treningowa
    train_pred <- predict(tree_info, newdata = train_data, type = "class")
    sum(train_pred == train_data$admit) / nrow(train_data)
    
    # Trafność testowa
    test_pred <- predict(tree_info, newdata = test_data, type = "class")
    sum(test_pred == test_data$admit) / nrow(test_data)
    
    # Prognoza dla nowych danych
    as.character(predict(tree_info, newdata = data.frame(gre = 700, gpa = 3.7, rank = 2), type = "class"))
    as.character(predict(tree_info, newdata = data.frame(gre = 650, gpa = 4.0, rank = 1), type = "class"))
    

# 4. Budowa drzewa regresyjnego dla danych Carseats
    
    # Budowa drzewa regresyjnego
    tree_reg <- rpart(Sales ~ ., data = Carseats)
    
    # Wizualizacja drzewa
    plot(tree_reg)
    text(tree_reg)
    
    # Wybór optymalnego cp
    optimal_tree <- prune(tree_reg, cp = tree_reg$cptable[which.min(tree_reg$cptable[, "xerror"]), "CP"])
    
    # Wizualizacja drzewa z optymalnym cp
    plot(optimal_tree)
    text(optimal_tree)
    

# 5. Budowa drzewa klasyfikacyjnego dla danych spam7
    
    # Podział danych na część uczącą i testową
    split <- sample.split(spam7$yesno, SplitRatio = 0.7)
    train_data <- subset(spam7, split == TRUE)
    test_data <- subset(spam7, split == FALSE)
    
    # Budowa drzewa klasyfikacyjnego
    tree_class <- rpart(yesno ~ ., data = train_data, method = "class")
    
    # Wizualizacja drzewa
    plot(tree_class)
    text(tree_class)
    
    # Przycięcie drzewa
    optimal_tree <- prune(tree_class, cp = 0.01)
    
    # Wizualizacja drzewa z optymalnym cp
    plot(optimal_tree)
    text(optimal_tree)
    
    # Trafność na zbiorze testowym
    table(predict(optimal_tree, newdata = test_data, type = "class"), test_data$yesno)
    
    
# 6. Budowa drzewa regresyjnego dla danych ames
    
    # Podział danych na zbiór treningowy i testowy
    split <- sample.split(ames$Sale_Price, SplitRatio = 0.7)
    train_data <- subset(ames, split == TRUE)
    test_data <- subset(ames, split == FALSE)
    
    # Budowa drzewa regresyjnego
    tree_reg <- rpart(Sale_Price ~ ., data = train_data)
    
    # Wybór optymalnego cp
    optimal_tree <- prune(tree_reg, cp = tree_reg$cptable[which.min(tree_reg$cptable[, "xerror"]), "CP"])
    
    # Podsumowanie i wizualizacja drzewa
    summary(optimal_tree)
    plot(optimal_tree)
    text(optimal_tree)
    
    # Prognoza dla zbioru testowego
    test_predictions <- predict(optimal_tree, newdata = test_data)
    
    # Błąd RMSE
    sqrt(mean((test_predictions - test_data$Sale_Price)^2))
    
    # Błąd MASE
    mean(abs(test_predictions - test_data$Sale_Price) / mean(abs(test_data$Sale_Price - mean(train_data$Sale_Price))))
    