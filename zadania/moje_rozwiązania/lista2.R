#install.packages(c("psych","ggpubr"))

library(ggpubr)
library(stats)
library(psych)
library(ggplot2)
library(expsmooth)
library(forecast)

# Dane
data <- c(-10, 9, 1, 2, 5, -2, 6, 2, 1, 0, 1, 4, 5, 6, 3, 7, 3, 2, 2, 3, 8, 5, 3, 4, 8, 0, 8, 0, 5, 1, 6, 4, 8, 13, 2, -13, 20) # 1,2,3,4 zadanie
data(mpg) # 5,6,7 zadanie
X <- c(1,5,10,8,9,1,2,4,5,6)
Y <- c(120, 115, 132, 123, 128, 102, 106, 109, 112, 110) # 8 zadanie
data(mtcars) # 9 zadanie


#1. Statystyki podstawowe dla wektora danych

    mean(data)
    median(data)
    sd(data)
    var(data)
    quantile(data)
    summary(data)
    min(data)
    max(data)
    
    describe(data)

    
#2. Histogram dla wektora danych

    hist(data, main="Histogram", xlab="Values", ylab="Frequency", col="lightblue")

    
#3. Boxplot dla wektora danych
    
    boxplot(data, main="Boxplot", ylab="Values")

    
#4. Identyfikacja wartości odstających w danych
    
    # Metoda 3 sigm
    data[data <= (mean(data) - 3*sd(data)) | data >= (mean(data) + 3*sd(data))]
    
    # Metoda półtora rozstępu ćwiartkowego 
    Q25 <- quantile(data, probs = 0.25)
    Q75 <- quantile(data, probs = 0.75)
    data[data <= (Q25 - 1.5*IQR(data)) | data >= (Q75 + 1.5*IQR(data))]
    
    
#5. Statystyki podstawowe dla kolumny 'hwy' w ramce danych 'mpg'
    
    summary(mpg$hwy)
    sd(mpg$hwy)
    var(mpg$hwy)
    median(mpg$hwy)
    quantile(mpg$hwy)

    
#6. Histogramy i boxplot dla kolumny 'hwy' w ramce danych 'mpg'
    
    hist(mpg$hwy, breaks = 10, main = "Histogram", xlab = "Highway miles per gallon",col = "green")
    hist(mpg$hwy, breaks = 5, main = "Histogram", xlab = "Highway miles per gallon",col = "red")
    boxplot(mpg$hwy, main = "Boxplot", ylab="Values")

    
#7. Identyfikacja wartości odstających w kolumnie 'hwy' w ramce danych 'mpg'

    # Metoda 3 sigm
    outliers_3sigma <- mpg$hwy[mpg$hwy <= (mean(mpg$hwy) - 3*sd(mpg$hwy)) | mpg$hwy >= (mean(mpg$hwy) + 3*sd(mpg$hwy))]
    which(mpg$hwy %in% outliers_3sigma)
    
    # Metoda półtora rozstępu ćwiartkowego 
    q25_hwy <- quantile(mpg$hwy, 0.25)
    q75_hwy <- quantile(mpg$hwy, 0.75)
    outliers_hwy <- mpg$hwy[mpg$hwy <= (q25_hwy - 1.5*IQR(mpg$hwy)) | mpg$hwy >= (q75_hwy + 1.5*IQR(mpg$hwy))]
    which(mpg$hwy %in% outliers_hwy)
    
    
#8. Wykres rozrzutu i obliczenie korelacji

    plot(X, Y)
    cor(x = X, y = Y)

    
#9. Analiza korelacji między 'mpg' a 'wt' w ramce danych 'mtcars'
    
    # Obliczenie współczynnika korelacji między 'mpg' a 'wt'
    cor(x = mtcars$mpg, y = mtcars$wt)
    
    # Model regresji liniowej między 'mpg' a 'wt'
    lm(wt ~ mpg, data = mtcars)
  
    # Wykres rozrzutu z regresją liniową
    ggscatter(mtcars, x = "mpg", y = "wt", add = "reg.line", xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")  

    
#10. Analiza opóźnień czasowych (lag plot) dla serii czasowych
    
    # Lag plot dla serii czasowej z 4 opóźnieniami
    lag.plot(bonds, lags = 4, do.lines = FALSE) 
    lag.plot(visitors, lags = 4, do.lines = FALSE)
    
    # Wykresy serii czasowej
    plot(bonds, main = "Time - Bonds", xlab = "Time", ylab = "Bonds")
    plot(visitors, main = "Time - Visitors", xlab = "Time", ylab = "Visitors")
    
    # Prognozy na 4 podokresy dla serii czasowej przy użyciu modelu regresji z trendem
    forecast_bonds <- forecast(tslm(bonds ~ trend), h = 4) 
    forecast_visitors <- forecast(tslm(visitors ~ trend), h = 4)  
    
    forecast_bonds
    forecast_visitors
    