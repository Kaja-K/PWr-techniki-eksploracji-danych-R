# install.packages("arules")

library(arules)

#1
    data("Groceries")
    
    item_frequency <- itemFrequency(Groceries)# Częstość produktów
    item_frequency_sorted <- item_frequency[order(item_frequency, decreasing = TRUE)]
    
    # Wykres częstości
      barplot(item_frequency_sorted[1:20], main = "Top 20 Product Frequencies", col = "skyblue", las = 2) 
    
    item_frequency_relative <- itemFrequency(Groceries, type = "relative")  # Częstość względna produktów
    item_frequency_relative_sorted <- item_frequency_relative[order(item_frequency_relative, decreasing = TRUE)]
    
    # Wykres częstości względnej
      barplot(item_frequency_relative_sorted[1:20], main = "Top 20 Relative Product Frequencies", col = "salmon", las = 2)

#2
    rules <- apriori(Groceries, parameter = list(support = 0.001, confidence = 0.8)) # Znalezienie reguł asocjacyjnych
    
    inspect(subset(rules, lhs %in% "whole milk" & rhs %in% "whole milk")) # Reguły dla "whole milk"
    inspect(subset(rules, lhs %in% c("rice", "sugar") & !rhs %in% c("rice", "sugar")))# Reguły dla "rice" i "sugar"