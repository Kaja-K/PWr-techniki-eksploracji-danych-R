# 1
    #http://www-eio.upc.edu/~pau/cms/rdata/datasets.html - dane
    
    data(package = "datasets")
    
    #install.packages("forecast")
    library(forecast)
    library(help="forecast")

    
#2
    data(package = "forecast")
    
    data(wineind)
    wineind
    
    class(wineind) # format
    length(wineind) # długość
    frequency(wineind) # częstotliwość - liczba obserwacji na jednostkę czasu
    start(wineind) # początek danych
    end(wineind) # koniec danych
    deltat(wineind) # odstęp pomiędzy obserwacjami
    time(wineind) # wektor punktów czasowych
    cycle(wineind) #pozycja operacji w cyklu

    
#3
    plot(wineind, main = "Wine Sales in Australia", xlab = "Time", ylab = "Wine Sales", col = "green")
    legend("topright", legend = c("Wine Sales"), col = "green", lty = 1, lwd = 2)

    
#4
    scanned_data <- scan("listy/dane/wine.txt")
    class(scanned_data)
    ts(scanned_data, start = c(1980, 1), frequency = 12) #częstotliwość 4 i wartości kwartalne
    as.numeric(ts_data) # numerycznie
    as.character(ts_data) # tekstowo

    
#5
    #http://stat.gov.pl/bdl/ - dane
    
    library(tidyverse)
    data <- read.csv("listy/dane/wolne_miejsca_pracy.csv", sep = ";", header = FALSE)
    
    col_names <- data[1, ] # ustawienia nazw kolumn
    data <- data[-1, ] #usunięcie nazw kolumn
    
    new_col_names <- c("Kod", "Nazwa", "2015", "2016", "2017", "2018", "2019", "2020", "2021","2022")
    colnames(data) <- new_col_names # ustawienie nowych nazw kolumn
    data <- data[, -ncol(data)] # usunięcie ostatniej kolumny z NA
    
    head(data)

    
#6
    data_txt <- read.table("listy/dane/wine_years.txt", header = TRUE)
    
    ts_data <- ts(data_txt[, 3:ncol(data_txt)], start = c(2015, 1), frequency = 1)
    ts_data
    summary(ts_data)# właściwości

    
#7
    #install.packages("tseries") # read.ts jest z tej bioblioteki
    library(tseries)
    write(ts_data, file = "listy/dane/wina.ts")
    data_ts <- read.ts("listy/dane/wine_years.txt",header = TRUE)
    data_ts

    
#8
    numeric_data <- rnorm(72)
    
    ts_data_12 <- ts(numeric_data, start = c(2010, 1), frequency = 12) # od stycznia 2010
    ts_data_4 <- ts(numeric_data, start = c(2010, 1), frequency = 4) # od I kw 2010
    ts_data_1 <- ts(numeric_data, start = c(2010, 1), frequency = 1) # od 2010
    
    #ts_data_12
    #ts_data_4
    #ts_data_1
    
    write(ts_data_12, file = "listy/dane/ts_data_12.ts")
    write(ts_data_4, file = "listy/dane/ts_data_4.ts")
    write(ts_data_1, file = "listy/dane/ts_data_1.ts")
    
    # as.ts() - przekształcenie innych obiektów na obiekt ts
    # ts() - tworzy obiekt szeregów czasowych z danych wyjściowych i określonych dat i częstotliwości

    
#9
    data_frame <- data.frame(data) # dane z miejsc pracy

    class(data_frame)
    sapply(data_frame, class) # klasa kolumn
    
    new_row <- c("0000001", "NOWY_WIERSZ", 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0)
    data_frame <- rbind(data_frame, new_row) #nowy wiersz
    data_frame$NOWA_KOLUMNA <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA, NA, NA, NA, NA, NA, NA, NA) # nowa kolumna

    ncol(data_frame)# liczba kolumn
    dim(data_frame) # wymiary ramki danych
    head(data_frame, 2) # dwa pierwsze wiersze
    tail(data_frame, 2) # dwa ostatnie wiersze
    str(data_frame) #skrócony opis typów danych
    
    
#10
    data("AirPassengers")
    
    subset_data <- window(AirPassengers, start = c(1950, 1), end = c(1957, 12))# zmiana na ts

    plot(subset_data, main = "AirPassengers: Jan 1950 - Dec 1957", ylab = "Air Passengers", xlab = "Time")
    
    
#11
    #install.packages("expsmooth")
    library(expsmooth)
    data(package = "expsmooth")
    data(usgdp)
    
    usgdp <- data.frame(usgdp)
    usgdp_ts <- ts(unlist(usgdp), start = c(1947, 1), frequency = 4) # zmiana na ts
    usgdp_data <- window(usgdp_ts, start = c(1950, 1), end = c(1957, 4))
    
    plot(usgdp_data, main = "US GDP",xlab = "Year", ylab = "GDP",col = "green", lty = 1, type = "o")
    grid()
    
    
#12
    #install.packages("quantmod",dependencies=TRUE)
    library(quantmod)
    
    getSymbols("^DJI", src = "yahoo", from = "2022-01-01", to = Sys.Date())
    getSymbols("USDEUR=X", src = "yahoo", from = "2022-01-01", to = Sys.Date())
    getSymbols("BTC-USD", src = "yahoo", from = "2022-01-01", to = Sys.Date())

    head(DJI)
    head(`USDEUR=X`)
    head(`BTC-USD`)
    
    class(DJI)
    class(`USDEUR=X`)    
    class(`BTC-USD`)
    
    
#13
    #install.packages("tseries")
    library(tseries)
    
    IBM <- get.hist.quote(instrument = "IBM", quote = c("Open", "High"), provider = "yahoo", start = "2015-07-01", end = "2020-08-06")
    
    head(IBM)
    class(IBM)

    
#14
    data("AirPassengers")
    
    subset_data <- window(AirPassengers, start = c(1950, 1), end = c(1957, 12)) # Obcięcie danych
    
    for (lag in 1:12) {lag.plot(AirPassengers, lags = lag, main = "Plots for AirPassengers Data")}
    for (lag in 1:12) {lag.plot(subset_data, lags = lag, main = "Plots for AirPassengers Data")}
    
    
#15
    white_noise <- rnorm(length(AirPassengers))
    
    plot(white_noise, type = "l", main = "White Noise", ylab = "Value", xlab = "Index")
    for (lag in 1:4) {lag.plot(white_noise, lags = lag, main = "White Noise")} # wykresy rozrzutu

    
#16   
    diff_data_1 <- diff(AirPassengers, lag = 1) # Zróżnicowanie danych z opóźnieniem 1
    diff_data_12 <- diff(AirPassengers, lag = 12) # Zróżnicowanie danych z opóźnieniem 12
    
    for (lag in 1:12) {lag.plot(diff_data_1, lags = lag, main = "Lag=1")}
    for (lag in 1:12) {lag.plot(diff_data_12, lags = lag, main = "Lag = 12")}
    

#17
    #install.packages("lattice")
    library(lattice)
    library(ggplot2)
    
    data("Nile")

    xyplot(Nile ~ time(Nile), main = "Wykres z różnymi proporcjami obrazu", aspect = "iso")

    
#18 
    xyplot(Nile ~ time(Nile) | cut(time(Nile), 3, overlap = 0.5), layout = c(3, 1),
           main = "Wykres z trzema panelami i częścią wspólną 50%")