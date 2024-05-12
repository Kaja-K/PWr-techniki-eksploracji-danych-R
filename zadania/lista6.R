# install.packages("arules")

library(arules)

# Powtarzalność podziałów
set.seed(123) 

# Dane
data("Groceries")


# 1. Analiza danych o zakupach - częstość produktów
    
    # Częstość produktów
    item_frequency <- itemFrequency(Groceries)
    item_frequency_sorted <- item_frequency[order(item_frequency, decreasing = TRUE)]
    
    # Wykres częstości
    barplot(item_frequency_sorted[1:20], main = "Top 20 Product Frequencies", col = "skyblue", las = 2) 
    
    # Częstość względna produktów
    item_frequency_relative <- itemFrequency(Groceries, type = "relative")
    item_frequency_relative_sorted <- item_frequency_relative[order(item_frequency_relative, decreasing = TRUE)]
    
    # Wykres częstości względnej
    barplot(item_frequency_relative_sorted[1:20], main = "Top 20 Relative Product Frequencies", col = "salmon", las = 2)


# 2. Znalezienie reguł asocjacyjnych dla danych o zakupach
    
    rules <- apriori(Groceries, parameter = list(support = 0.001, confidence = 0.8))
    
    # Reguły dla "whole milk" oraz "rice" i "sugar"
    inspect(subset(rules, lhs %in% "whole milk"))    
    inspect(subset(rules, lhs %in% c("rice", "sugar") & !rhs %in% c("rice", "sugar")))
    