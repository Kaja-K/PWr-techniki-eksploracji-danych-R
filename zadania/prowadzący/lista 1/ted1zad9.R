imiona <- c("Jan", "Fiona", "Zofia")
wiek <- c(25, 23, 33)
płeć=c("m", "f", "f")
płeć=factor(płeć, levels=c("m", "f"))
ramka <- data.frame(imiona, wiek, płeć)
ramka
#od 4.0.0 domyslne FALSE dla strinsAsFactors
#The main difference is that factors have predefined levels. 
#Thus their value can only be one of those levels or NA. Whereas characters can be anything.
# pobieramy 1 i 2 wiersz
ramka[c(1, 2), ]
# pierwsza kolumna bez drugiego wiersza
ramka[-2, 1]
# bez drugiego wiersza
ramka[-2, ]
# tylko druga kolumna
ramka[, 2]
#pobieramy kolumne 'wiek' po nazwie
ramka$wiek
# inny sposob indeksowanie po nazwie
ramka[, "wiek"]
# osoby ponizej 30 roku ?ycia
ramka[ramka$wiek < 30, ]
# dane tylko Zofii
ramka[ramka$imiona == "Zofia", ]
ramka[imiona=="Zofia",]
# analogicznie dla wektorow
wiek <- ramka$wiek
wiek[wiek < 30]
# tworzymy nowy wiersz jako ramke danych
nowy_wiersz <- data.frame(imiona = "Patryk", wiek = 25, płeć="m")
nowy_wiersz=c("Fred", 26, "m")
# laczenie ramki danych z nowym wierszem
ramka <- rbind(ramka, nowy_wiersz)
ramka
# dodanie nowej kolumny
ocena <- c(4,5,4)
ramka <- cbind(ramka, ocena)
ramka
ramka$ocena=factor(ramka$ocena, levels=c(2,3,4,5), ordered = T)
# dodanie nowego wiersza przy uzyciu indeksowania
ramka[5, 1] <- "Radek"
ramka[5, 2] <- 27
ramka[5, 3] <- "M"
ramka
# liczba kolumn w ramce
length(ramka)
# rozmiary ramki: 5 wierszy, 3 kolumny
dim(ramka)
# skr?=ocony opis typow danych w ramce
str(ramka)
# wyswietlenie pierwszych n wierszy (domyslnie n = 6)
head(ramka, n = 2)
# wyswietlenie ostatnich n wierszy (domyslnie n = 6)
tail(ramka, n = 2)