#install.packages("psych")
#install.packages("ggpubr")

library(ggpubr)
library(stats)
library(psych)
library(ggplot2)
library(expsmooth)
library(forecast)


#1
    data <- c(-10, 9, 1, 2, 5, -2, 6, 2, 1, 0, 1, 4, 5, 6, 3, 7, 3, 2, 2, 3, 8, 5, 3, 4, 8, 0, 8, 0, 5, 1, 6, 4, 8, 13, 2, -13, 20)
    
    mean(data)
    median(data)
    sd(data)
    var(data)
    quantile(data)
    summary(data)
    min(data)
    max(data)
    
    describe(data)

    
#2
    hist(dane, main="Histogram", xlab="Values", ylab="Frequency", col="lightblue")

    
#3
    boxplot(data, main="Boxplot", ylab="Values")

    
#4
    # Metoda 3 sigm
    data[data <= (mean(data) - 3*sd(data)) | data >= (mean(data) + 3*sd(data))]
    
    #Metoda półtora rozstępu ćwiartkowego 
    Q25 <- quantile(data, probs = 0.25)
    Q75 <- quantile(data, probs = 0.75)
    data[data <= (Q25 - 1.5*IQR(data)) | data >= (Q75 + 1.5*IQR(data))]
    
    
#5
    data(mpg)
    
    summary(mpg$hwy)
    sd(mpg$hwy)
    var(mpg$hwy)
    median(mpg$hwy)
    quantile(mpg$hwy)

    
#6
    hist(mpg$hwy, breaks = 10, main = "Histogram", 
         xlab = "Highway miles per gallon",col = "green")
    hist(mpg$hwy, breaks = 5, main = "Histogram", 
         xlab = "Highway miles per gallon",col = "red")
    
    boxplot(mpg$hwy, main = "Boxplot", ylab="Values")

    
#7
    #Metoda 3 sigm
      outliers_3sigma <- mpg$hwy[mpg$hwy <= (mean(mpg$hwy) - 3*sd(mpg$hwy)) | mpg$hwy >= (mean(mpg$hwy) + 3*sd(mpg$hwy))]

    which(mpg$hwy %in% outliers_3sigma)
    
    #Metoda półtora rozstępu ćwiartkowego 
      q25_hwy <- quantile(mpg$hwy, 0.25)
      q75_hwy <- quantile(mpg$hwy, 0.75)
      outliers_hwy <- mpg$hwy[mpg$hwy <= (q25_hwy - 1.5*IQR(mpg$hwy)) | mpg$hwy >= (q75_hwy + 1.5*IQR(mpg$hwy))]
      
    which(mpg$hwy %in% outliers_hwy)

    
#8

    X <- c(1,5,10,8,9,1,2,4,5,6)
    Y <- c(120, 115, 132, 123, 128, 102, 106, 109, 112, 110)
    
    plot(X, Y)
    cor(x = X, y = Y,)

    
#9
    data(mtcars)
    
    cor(x = mtcars$mpg, y = mtcars$wt)
    lm(wt ~ mpg, data = mtcars) # regresja liniowa

    ggscatter(mtcars, x = "mpg", y = "wt", add = "reg.line", xlab = "Miles/(US) gallon", 
              ylab = "Weight (1000 lbs)") # wykres rozrzutu 

    
#10
    lag.plot(bonds, lags = 4, do.lines = FALSE) # wykres rozrzutu dla opóźnień
    lag.plot(visitors, lags = 4, do.lines = FALSE)
    
    plot(bonds, main = "Time - Bonds", xlab = "Time", ylab = "Bonds")
    plot(visitors, main = "Time - Visitors", xlab = "TIme", ylab = "Visitors")
    
    forecast(tslm(bonds ~ trend), h = 4) # prognoza na 4 podokresy
    forecast(tslm(visitors ~ trend), h = 4) 