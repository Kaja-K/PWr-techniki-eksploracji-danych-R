library(rpart)
library(rpart.plot)
library(party)
set.seed(1)#ustawienie losowości aby mieć te same wyniki
indeks=sample(200, 170)
reading.train=readingSkills[indeks, ]
reading.test=readingSkills[-indeks, ]
mojedrzewo=rpart(nativeSpeaker~score+age, 
                 data=reading.train, method="class",
                 parms=list(split="information")) #drzewo z atrybutem decyzyjnym nativeSpeaker i atrybutami warunkowymi age i score
#drzewo klasyfikacyjne skonstruoane za pomoca zysku informacyjnego (entropii)
rpart.plot(mojedrzewo)
accur=function(x){sum(diag(x))/sum(rowSums(x))*100}
prognozy=predict(mojedrzewo, newdata=reading.test, type="class") #progoza dla zbioru testowego
a=table(prognozy, reading.test$nativeSpeaker) #tablica pomyłek, wiersz prawidłowe ze zbioru testowego
a
accur(a) #trafność
100-accur(a) #błąd testowy

prognozy=predict(mojedrzewo, newdata=reading.train, type="class") #prognoza dla zbioru treningowego
a=table(prognozy, reading.train$nativeSpeaker)
a
accur(a)
100-accur(a) #błąd treningowy

mojedane=data.frame(age=c(11,7), shoeSize=c(NA, NA), 
                    score=c(34.45628, 36.34538)) #2 obiekty dla których chcemy podać prognozę
predict(mojedrzewo, newdata=mojedane, type="class")

mojedrzewo=rpart(nativeSpeaker~., 
                 data=reading.train, method="class",
                 parms=list(split="information")) #drzewo z atrybutem decyzyjnym nativeSpeaker i atrybutami warunkowymi age i score
#drzewo klasyfikacyjne skonstruoane za pomoca zysku informacyjnego (entropii)
rpart.plot(mojedrzewo)

