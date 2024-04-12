#Zadania zaliczeniowe. Na pytania z obu zadan prosze odpowiedziec w oparciu o wykonanie odpowiednich analiz i testow statystycznych. Dodatkowo prosze scharakteryzowac rozklady dla poszczegolnych zbiorow danych w oparciu o analize histogramow (tj. opisac modalnosc, a w przypadku zbiorow jednomodalnych takze splaszczenie i symetrie podajac wartosci liczbowe tych charakterystyk).
library("tidyverse")
library("modeest")
library("moments")
library("Rmisc")

#zadanie 1. Przygotowanie danych
gold <- read.csv("gold.csv", header = TRUE, sep = ";")
prox <- gold %>% 
  filter(Sampling.site == "Proximal")
prox.gold <- prox$n_of_gold_grains #zawartosc zlota w probkach stozka proksymalnego
prox.magnet <- prox$Magnetic_.fraction_g_m3 #zawartosc frakcji magnetycznej w probkach stozka prksymalnego 
prox.paramagnet <- prox$Paramagnetic_fraction_g_m3 #zawartosc frakcji paramagnetycznej w probkach stozka proksymalnego 
prox.nonmagnet <- prox$Non_magnetic_fraction_g_m3 #zawartosc mineralow  w probkach stozka prksymalnego 

#to samo co wyzej, tylko dla stozka dystalnego
dist <- gold %>% 
  filter(Sampling.site == "Distal")
dist.gold <- dist$n_of_gold_grains
dist.magnet <- dist$Magnetic_.fraction_g_m3
dist.paramagnet <- dist$Paramagnetic_fraction_g_m3
dist.nonmagnet <- dist$Non_magnetic_fraction_g_m3

#to samo co wyzej, tylko dla osadow ze strumienia
stream <- gold %>% 
  filter(Sampling.site == "Stream")
stream.gold <- stream$n_of_gold_grains
stream.magnet <- stream$Magnetic_.fraction_g_m3
stream.paramagnet <- stream$Paramagnetic_fraction_g_m3
stream.nonmagnet <- stream$Non_magnetic_fraction_g_m3

#Zadanie 1, pytania:
#Czy osady stożka proksymalnego i dystalnego różnią się w sposób istotny statystycznie zawartością złota?
#Czy osady stożka dystalnego oraz osadów strumieniowych różnią się w sposób istotny statystycznie zawartością frakcji magnetycznej?
#Czy osady stożka proksymalnego i dystalnego różnią się w sposób istotny statystycznie zawartością frakcji paramagnetycznej?
#Czy zawartość złota jest powiązana z obecnością frakcji magnetycznej? Poszukaj tego związku dla prób ze stożka proksymalnego.
#Czy zawartość złota jest powiązana z obecnością frakcji magnetycznej? Poszukaj tego związku dla prób ze stożka dystalnego.
#Czy zawartość złota jest powiązana z obecnością frakcji paramagnetycznej? Poszukaj tego związku dla prób ze stożka proksymalnego.
#Czy zawartość złota jest powiązana z obecnością minerałów ciężkich niemagnetycznych? Poszukaj tego związku dla prób ze stożka proksymalnego.

#zadanie 2. Przygotowanie danych
bazalty <- read.csv("bazalty.csv", header = TRUE, sep = ";")
#dane dla stanowiska 74-527
S527 <- bazalty %>% 
  filter(Stanowisko == "74-527")
S527.TiO2 <- S527$TiO2_perc
S527.Al2O3 <- S527$Al2O3_perc
S527.Fe2O3 <- S527$Fe2O3_perc
S527.MgO <- S527$MgO_perc
S527.CaO <- S527$CaO_perc

#dane dla stanowiska 74-528
S528 <- bazalty %>% 
  filter(Stanowisko == "74-528")
S528.TiO2 <- S528$TiO2_perc
S528.Al2O3 <- S528$Al2O3_perc
S528.Fe2O3 <- S528$Fe2O3_perc
S528.MgO <- S528$MgO_perc
S528.CaO <- S528$CaO_perc

#zadanie 2, pytania: 
#	Czy bazalty ze stanowisk 74-527 i 74-528 różnią się w sposób istotny zawartością TiO2?
#	Czy bazalty ze stanowisk 74-527 i 74-528 różnią się w sposób istotny zawartością FeO?
#	Czy bazalty ze stanowisk 74-527 i 74-528 różnią się w sposób istotny zawartością Al2O3?
#	Czy bazalty ze stanowisk 74-527 i 74-528 różnią się w sposób istotny zawartością CaO?
#	Czy zawartość Al2O3 ma związek z zawartością MgO?
#	Czy zawartość TiO2 ma związek z zawartością CaO?
