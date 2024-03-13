#install.packages(c("forecast", "quantmod", "tseries", "expsmooth", "lattice"))

library(forecast) 
library(tidyverse)
library(quantmod)
library(tseries)
library(expsmooth)
library(lattice)
library(ggplot2)
library(tseries)
    

# 1. Wyświetlenie dostępnych danych w pakiecie "forecast"
    
    #http://www-eio.upc.edu/~pau/cms/rdata/datasets.html - Dane
    data(package = "datasets")
    library(help = "forecast")
    

# 2. Przykład zastosowania danych wineind
    
    data(package = "forecast")
    data(wineind)
    
    class(wineind) # Format danych
    length(wineind) # Długość danych
    frequency(wineind) # Częstotliwość danych (liczba obserwacji na jednostkę czasu)
    start(wineind) # Początek danych
    end(wineind) # Koniec danych
    deltat(wineind) # Odstęp pomiędzy obserwacjami
    time(wineind) # Wektor punktów czasowych
    cycle(wineind) # Pozycja operacji w cyklu

    
# 3. Wykres danych wineind
    
    plot(wineind, main = "Wine Sales in Australia", xlab = "Time", ylab = "Wine Sales", col = "green")
    legend("topright", legend = c("Wine Sales"), col = "green", lty = 1, lwd = 2)

    
# 4. Skanowanie danych z pliku tekstowego

    scanned_data <- scan("listy/dane/wine.txt")
    class(scanned_data)
    ts_data <- ts(scanned_data, start = c(1980, 1), frequency = 12) # Częstotliwość 12 (miesięczne)
    as.numeric(ts_data) # Numerycznie
    as.character(ts_data) # Tekstowo

  
# 5. Przetwarzanie danych z pliku CSV
    
    #http://stat.gov.pl/bdl/ - Dane
    data <- read.csv("listy/dane/wolne_miejsca_pracy.csv", sep = ";", header = FALSE)
    
    col_names <- data[1, ] # Ustawienia nazw kolumn
    data <- data[-1, ] # Usunięcie pierwszego wiersza z nazwami kolumn
    new_col_names <- c("Kod", "Nazwa", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
    colnames(data) <- new_col_names # Ustawienie nowych nazw kolumn
    data <- data[, -ncol(data)] # Usunięcie ostatniej kolumny z N
    
    head(data) # Wyświetlenie wyniku 

    
# 6. Wczytanie danych z pliku tekstowego i utworzenie szeregów czasowych

    data_txt <- read.table("listy/dane/wine_years.txt", header = TRUE)
    
    ts_data <- ts(data_txt[, 3:ncol(data_txt)], start = c(2015, 1), frequency = 1)
    ts_data
    summary(ts_data) # Właściwości szeregów czasowych

    
#7, Zapisanie i odczytanie szeregów czasowych
    
    write(ts_data, file = "listy/dane/wina.ts")
    data_ts <- read.ts("listy/dane/wine_years.txt",header = TRUE)
    data_ts

    
# 8. Przykłady tworzenia szeregów czasowych o różnej częstotliwości

    numeric_data <- rnorm(72)
    ts_data_12 <- ts(numeric_data, start = c(2010, 1), frequency = 12) # Miesięczne
    ts_data_4 <- ts(numeric_data, start = c(2010, 1), frequency = 4)  # Kwartałowe
    ts_data_1 <- ts(numeric_data, start = c(2010, 1), frequency = 1)  # Roczne
    
    write(ts_data_12, file = "listy/dane/ts_data_12.ts")
    write(ts_data_4, file = "listy/dane/ts_data_4.ts")
    write(ts_data_1, file = "listy/dane/ts_data_1.ts")
    
    # as.ts() - przekształcenie innych obiektów na obiekt ts
    # ts() - tworzy obiekt szeregów czasowych z danych wyjściowych i określonych dat i częstotliwości

    
# 9. Manipulacje danymi w ramce danych

    data_frame <- data.frame(data)
    class(data_frame)
    sapply(data_frame, class) # Klasy kolumn
    
    new_row <- c("0000001", "NOWY_WIERSZ", 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0)
    data_frame <- rbind(data_frame, new_row) # Dodanie nowego wiersza
    data_frame$NOWA_KOLUMNA <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA, NA, NA, NA, NA, NA, NA, NA) # Dodanie nowej kolumny
    
    ncol(data_frame) # Liczba kolumn
    dim(data_frame)  # Wymiary ramki danych
    head(data_frame, 2) # Pierwsze dwa wiersze
    tail(data_frame, 2) # Ostatnie dwa wiersze
    str(data_frame) # Skrócony opis typów danych
    
    
# 10. Wyświetlanie danych AirPassengers

    data("AirPassengers")
    subset_data <- window(AirPassengers, start = c(1950, 1), end = c(1957, 12))
    plot(subset_data, main = "AirPassengers: Jan 1950 - Dec 1957", ylab = "Air Passengers", xlab = "Time")
    
    
# 11. Wyświetlanie danych US GDP i zrobienie wykresu
    
    data(package = "expsmooth")
    data(usgdp)
    usgdp <- data.frame(usgdp)
    usgdp_ts <- ts(unlist(usgdp), start = c(1947, 1), frequency = 4)
    usgdp_data <- window(usgdp_ts, start = c(1950, 1), end = c(1957, 4))
    plot(usgdp_data, main = "US GDP", xlab = "Year", ylab = "GDP", col = "green", lty = 1, type = "o")
    grid()
    
    
# 12. Pobieranie danych giełdowych

    dane <- getSymbols("^DJI", src = "yahoo", from = "2022-01-01", to = Sys.Date(), auto.assign = FALSE)
    getSymbols("USDEUR=X", src = "yahoo", from = "2022-01-01", to = Sys.Date())
    getSymbols("BTC-USD", src = "yahoo", from = "2022-01-01", to = Sys.Date())
    head(DJI)
    head(`USDEUR=X`)
    head(`BTC-USD`)
    class(DJI)
    class(`USDEUR=X`)
    class(`BTC-USD`)

    plot(DJI)
    plot(dane)
    
# 13. Pobieranie danych giełdowych z instrumentem IBM

    IBM <- get.hist.quote(instrument = "IBM", quote = c("Open", "High"), 
                          provider = "yahoo", start = "2015-07-01", end = "2020-08-06")
    head(IBM)
    class(IBM)

        
# 14. Wykresy opóźnień dla danych AirPassengers
    
    for (lag in 1:12) {lag.plot(AirPassengers, lags = lag, main = "Plots for AirPassengers Data")}
    for (lag in 1:12) {lag.plot(subset_data, lags = lag, main = "Plots for AirPassengers Data")}
    

# 15. Wykres białego szumu    
    
    white_noise <- rnorm(length(AirPassengers))
    plot(white_noise, type = "l", main = "White Noise", ylab = "Value", xlab = "Index")
    for (lag in 1:4) {lag.plot(white_noise, lags = lag, main = "White Noise")} # Wykresy rozrzutu

    
# 16. Różnicowanie szeregów czasowych 
    
    diff_data_1 <- diff(AirPassengers, lag = 1) # Różnicowanie danych z opóźnieniem 1
    diff_data_12 <- diff(AirPassengers, lag = 12) # Różnicowanie danych z opóźnieniem 12
    
    for (lag in 1:12) {lag.plot(diff_data_1, lags = lag, main = "Lag=1")}
    for (lag in 1:12) {lag.plot(diff_data_12, lags = lag, main = "Lag = 12")}
    

# 17. Wykresy dla danych Nile

    data("Nile")
    xyplot(Nile ~ time(Nile), main = "Wykres z różnymi proporcjami obrazu", aspect = "iso")
    
    
# 18. Podział wykresu Nile na trzy panele z częścią wspólną
    
    xyplot(Nile ~ time(Nile) | cut(time(Nile), 3, overlap = 0.5), layout = c(3, 1),
           main = "Wykres z trzema panelami i częścią wspólną 50%")