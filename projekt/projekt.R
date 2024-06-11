library(stats)
library(ggplot2)
library(tidyverse)
library(rpart)
library(rpart.plot)

# Wczytanie danych
dane <- read.csv("dane/tips.csv", sep = ",", header = TRUE)
dane$X <- NULL

# Konwersja danych na faktory z polskimi etykietami
names(dane) <- c("Rachunek", "Napiwek", "Płeć", "Palacz", "Dzień", "Pora", "Rozmiar")
dane$Płeć <- factor(dane$Płeć, levels = c("Female", "Male"), labels = c("Kobieta", "Mężczyzna"))
dane$Dzień <- factor(dane$Dzień, levels = c("Thur", "Fri", "Sat", "Sun"), labels = c("Czwartek", "Piątek", "Sobota", "Niedziela"))
dane$Pora <- factor(dane$Pora, levels = c("Lunch", "Dinner"), labels = c("Lunch", "Obiad"))
dane$Palacz <- factor(dane$Palacz, levels = c("Yes", "No"), labels = c("Tak", "Nie"))


# OPIS DANYCH

  head(dane)                                                
  sapply(dane, typeof)                                       
  sapply(dane, class)                                          
  sapply(dane[, c("Płeć", "Dzień", "Pora", "Rozmiar")], unique)  
  sapply(dane[, c("Rachunek", "Napiwek")], range)               
  str(dane)                                                    
  summary(dane[,sapply(dane, is.numeric)])                  
  


# WYKRESY

  # Pudełko - Wąsy
  ggplot(dane, aes(x = Płeć, y = Napiwek, fill = Płeć)) +
    geom_boxplot(color = "black", alpha = 0.7) +
    scale_fill_manual(values = c("Mężczyzna" = "lightblue", "Kobieta" = "pink")) +
    labs(title = "Rozkład napiwków", x = "", y = "Wartość napiwku") +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16), axis.text.x = element_blank())
  
  # Histogram 
  ggplot(dane, aes(x = Napiwek, fill = Płeć)) +
    geom_histogram(bins = 30, color = "black", alpha = 0.7, position = "identity") +
    facet_wrap(~Płeć, scales = "fixed") +
    labs(title = "Rozkład napiwków z podziałem na płeć", x = "Wartość napiwku", y = "Liczebność napiwków") +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 14), strip.text.x = element_blank()) + 
    scale_fill_manual(values = c("pink", "lightblue")) 
  
  
# ANALIZA DANYCH
  
  # Podział danych na zbiór treningowy i testowy
  set.seed(1) 
  indeksy <- sample(1:nrow(dane), 0.7 * nrow(dane))  # Losowy podział danych
  dane_treningowe <- dane[indeksy, ]  
  dane_testowe <- dane[-indeksy, ]  
  head(dane_testowe)
  head(dane_treningowe)
  
  # Budowa drzewa regresyjnego
  model_drzewa <- rpart(Napiwek ~ Płeć + Palacz + Rozmiar, data = dane_treningowe, method = "anova", control=rpart.control(cp=.0001))
  printcp(model_drzewa)
  
  # Wizualizacja drzewa 
  rpart.plot(
    model_drzewa,          # Model drzewa do wizualizacji
    type = 2,              # Prostszy styl wykresu
    extra = 101,           # Wyświetlanie numerów węzłów i liczby obserwacji
    under = TRUE,          # Tekst pod węzłami
    fallen.leaves = TRUE,  # Liście na dole
    cex = 1.2,             # Skalowanie tekstu
    branch.lty = 1,        # Typ linii węzłów
    branch = 0.5,          # Grubość linii węzłów
    box.palette = "Blues", # Kolor węzłów
    shadow.col = "gray",   # Cień wokół boxów
    nn = TRUE)             # Wyświetlanie numerów węzłów
  
  # Przewidywanie wartości napiwków na zbiorze treningowym i obliczenie MSE
  trening_napiwek <- predict(model_drzewa, newdata = dane_treningowe)
  mse_treningowy <- mean((dane_treningowe$Napiwek - trening_napiwek)^2)

  # Przewidywanie wartości napiwków na zbiorze testowym i obliczenie MSE
  test_napiwek <- predict(model_drzewa, newdata = dane_testowe)
  mse_testowy <- mean((dane_testowe$Napiwek - test_napiwek)^2)

  # Prognoza dla przykładowych danych
  przykladowe_dane <- data.frame(Palacz = c("Tak","Nie"), Płeć = c("Kobieta", "Mężczyzna"),Rozmiar = c(2,4))
  prognozy <- predict(model_drzewa, newdata = przykladowe_dane)
  
  # Wyświetlenie wyników
  cat("Błąd treningowy (MSE):", mse_treningowy, "\n")
  cat("Błąd testowy (MSE):", mse_testowy, "\n")
  cat("Prognozy dla przykładowych danych:", round(prognozy, 2), "\n")
