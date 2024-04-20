getwd() #określa katalog roboczy
setwd("C:/Users/Zbigniew Michna/Dokumenty/ProgramR") #zmienia katalog roboczy
#zapisz plik w notatniku - liczby odzielone spacjami i dziesiętne przecinkami
mojedane=scan("danemoje.txt", dec=",", sep=" ") #wczytuje plik do pamięci roboczej R
ls() #wyświetla obiekty w pamięci roboczej R
class(mojedane)
mojedane1=as.character(mojedane)
mojedane1
mojedane2=ts(mojedane1, frequency = 4, start=c(2023,1))
class(mojedane2)
plot(mojedane)
plot(mojedane2)
