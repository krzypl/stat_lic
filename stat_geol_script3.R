#testy statystyczne i korelacje
#testy statystyczne porownujace srednie mowia nam o tym, czy srednie jakiejs cechy z porownywanych populacji roznia sie w sposob istotny statystycznie. Co wazne porownujemy te sama ceche (np. porownanie zawartosci kwarcu w dwoch odmianach skal krystalicznych).
#to jaki test statystyczny wykonujemy dla porownania srednich zalezy od cech porownywanych populacji, dlatego przed wykonaniem samego testu musimy poznac cechy badanych zbiorow
#jednym z najpowszechniej wykorzystywanych testow jest test t (lub t-Studenta). Zeby jednak mozna go przeprowadzic badane zbiory musza posiadac rozklad normalny, te sama wariancje (miara rozrzutu wokol wartosci sredniej) oraz liczbe obserwacji
#generujemy dwie proby statystyczne, ktore bedziemy porownywac
dane.norm1 <- rnorm(150, mean = 8, sd = 1) 
dane.norm2 <- rnorm(150, mean = 4, sd = 1)
hist(dane.norm1)
hist(dane.norm2)
#sprawdzenie, czy zbiory posiadaja rozklad normalny
shapiro.test(dane.norm1)
shapiro.test(dane.norm2) #test Shapiro-Wilka pokazuje, ze obie proby statystyczne posiadaja cechy rozkladu normalnego (wartosci p sa wieksze od 0.05).

#Dla przypadkow, w ktorych test na normalnosc rozkladow wyszedl pozytywnie, przechodzimy do sprawdzenia, czy zbiory roznia sie istotnie statystycznie pod wzgledem wariancji. Do tego celu sluzy test F na rownosc wariancji

var.test(dane.norm1, dane.norm2) #wartosc F to stosunek wariancji porownywanych zbiorow. Najwazniejsza jednak jest wartosc p. Kiedy jest ona wieksza niz 0.05, tak jak w tym przypadku, znaczy to, ze mozemy przyjac, ze wariancja w obu populacjach jest taka sama. Pozytywny wynik testu pozwala na przeprowadzenie testu t Studenta

#test t Studenta. Po potwierdzeniu normalnosci rozkladu i jednakowosci wariancji mozna przeprowadzic test t, ktory pozwoli na stwierdzenie, czy srednie porownywanych zmiennych roznia sie od siebie istotnie statystycznie
t.test(dane.norm1, dane.norm2, var.equal = TRUE) #podobnie jak w poprzednich testach najistotniejsza jest wartosc p. W tym przypadku jesli jest ona mniejsza od 0.05, to mozemy mowic ze srednie populacji roznia sie od siebie istotnie statystycznie. W przyciwnym wypadku roznica jest nieistotna statystycznie. 

#wariancja nie zawsze jest rowna w porownywanych zbiorach. Jesli mamy do czynienia z taka sytuacja stosujemy zmodyfikowana wersje testu t, tzw. test t Welcha, ktory chociaz wciaz wymaga aby rozklady byly normalne, to nie jest wrazliwy na roznice w wariancji, a nawet na roznice w liczebnosci prob
dane.norm3 <- rnorm(150, mean = 4, sd = 1) 
dane.norm4 <- rnorm(120, mean = 4, sd = 6) #generujemy dwa zbiory o roznej wariancji
hist(dane.norm3)
hist(dane.norm4)

shapiro.test(dane.norm3)
shapiro.test(dane.norm4) #test na normalnosc rozkladow daje wynik pozytywny

var.test(dane.norm3, dane.norm4) #test F na jednakowosc wariancji daje wynik negatywny, wiec decydujemy sie na przeprowadzenie testu t Welcha

t.test(dane.norm3, dane.norm4, var.equal = FALSE) #wynik interpretujemy tak samo jak przy zwyklym tescie t

#test U Manna-Whitneya pozwala na przeprowadzenie testu w przypadku, kiedy jeden badz oba porownywane zbiory nie maja cech rozkladu normalnego. Trzeba jednak pamietac, ze jego wiarygodnosc jest mniejsza niz testu t. test Manna-Whitneya porownuje kazda obserwacje pod wzgledem mediany (test t wykorzystuje srednia) i bazuje na rangach. Zbiory nie musza byc tej samej wielkosci
dane.lnorm1 <- rlnorm(150, meanlog = 2, sdlog = 1)
dane.lnorm2 <- rlnorm(110, meanlog = 2, sdlog = 2) #generujemu zbiory o rozkladzie antymodalnym
hist(dane.lnorm1)
hist(dane.lnorm2)

shapiro.test(dane.lnorm1)
shapiro.test(dane.lnorm2) #test Shapiro-Wilka potwierdza, ze rozklady zbiorow nie maja cech rozkladu normalnego. W zwiazku z tym decydujemy sie na wykonanie testu U

wilcox.test(dane.lnorm1, dane.lnorm2) #podbnie jak w przypadku testu t, wartosc p ponizej 0.05 wskazuje na istotna statystycznie roznice miedzy zbiorami

#########korelacja
#analiza korelacji mowi nam czy porownywane cechy (dwie rozne, czyli inaczej niz w przypadku wczesniej opisywanych testow) sa ze soba istotnie statystycznie powiazane. Wspolczynniki korelacji wahaja sie w przedziale od -1 do 1. Wrtosci bliskie 0  wskazuja na brak powiazania, wartosci ujemne na powiazanie odwrotnie proporcjonalne, a wartosci dodatnie na powiazanie wprost proporcjonalne. Brak jest uniwersalnych kryteriow pozwalajacych na okreslenie, jaka wartosc wskazuje na silna korelacje. UWAGA: korelacja nie pozwala na wskazanie zwiazkow przyczynowo-skutkowych, mowi nam jedynie o zwiazku/wspolwystepowaniu dwoch zmiennych!

#najczescie wykorzystuje sie wspolczynniki korelacji Pearsona i Spearmana. W obu przypadkach korelacje oznaczamy litera r (np. korelacja r = 0.6)

#wspolczynnik korelacji Pearsona - stosujemy, kiedy oba zbiory maja rozklad normalny i brak jest wartosci odstajacych
kor.pearson1 <- sort(rnorm(50))
kor.pearson2 <- sort(rnorm(50)) #dane silnie skorelowane o rozkladzie normalnym
hist(kor.pearson1)
hist(kor.pearson2)

shapiro.test(kor.pearson1)
shapiro.test(kor.pearson2) #test potwierdza rozklad normalny

cor(kor.pearson1, kor.pearson2, method = "pearson") #kwspolczynnik osiaga wartosc bliska 1 mowiaca nam o tym, ze istnieje silne powiazanie miedzy dwoma zmiennymi
plot(kor.pearson1, kor.pearson2) #silna korelacja jest tez widoczna na wykresie punktowym zestawiajacym obserwacje obu zbiorow
cor.test(kor.pearson1, kor.pearson2, method = "pearson") #przeprowadzamy test na istotnosc statystyczna korelacji. Wartosc p jest nizsza niz 0.05 co wskazuje, ze korelacja jest istotna statystycznie

set.seed(12)
nkor.pearson1 <- rnorm(50)
set.seed(13)
nkor.pearson2 <- rnorm(50) #dane slabo skorelowane

shapiro.test(nkor.pearson1)
shapiro.test(nkor.pearson2) #test potwierdza normalnosc rozkladu

cor(nkor.pearson1, nkor.pearson2, method = "pearson") #wspolczynnik osiaga wartosci niewiele odbiegajace od 0, co wskazuje na slabe powiazania miedzy dwiema zmiennymi

plot(nkor.pearson1, nkor.pearson2) #wykres punkotwy pokazuje, ze obserwacje sa mocno rozrzucone

cor.test(nkor.pearson1, nkor.pearson2, method = "pearson") #test korelacji wskazuje, ze jest ona nieistotna statystycznie

#Jesli zbiory nie posiadaja rozkladu normalnego lub zaobserwowalismy obserwacje odstajace, to obliczamy wspolczynnik korelacji Spearmana, ktory bazuje na rangach przypisanych obserwacja
kor.spearman1 <- c(-20:3, rnorm(50, mean = 3, sd = 1), 3:30, 45)
kor.spearman2 <- c(-10:13, rnorm(50, mean = 3, sd = 5), 3:31)

boxplot(kor.spearman1)
boxplot(kor.spearman2) #oba zbiory posiadaja wartosci odstajace

shapiro.test(kor.spearman1)
shapiro.test(kor.spearman2) #test potwierdza, ze oba rozklady nie posiadaja cech rozkladu normalnego

cor(kor.spearman1, kor.spearman2, method = "spearman") #wspolczynnik osiaga wysokie wartosci, wskazujace wiec na silna korelacje

plot(kor.spearman1, kor.spearman2) #interpretacja stopnia korelacji miedzy zbiorami nie posiadajacymi cech rozkladow normalnych z wykresu punktowego nie zawsze jest mozliwa

cor.test(kor.spearman1, kor.spearman2, method = "spearman")

###Zadanie 2

gr1_a <- c(47, 27, 52, 30, 26.5, 49.5, 36, 12.5, 10.5, 11.5, 12, 33, 29.5, 22.5, 37, 22, 69, 46, 50, 40.5, 36.5, 25, 30.5, 28, 33.5, 16.5, 18, 16, 18.5, 14.5)
gr1_b <- c(23.1, 12.2, 34.5, 27.2, 15.5, 26.2, 30.2, 9.5, 8.5, 10, 11.2, 27.5, 17.3, 21.5, 36, 17.7, 33, 44.5, 28.5, 24, 24.5, 13, 21.7, 21, 22.5, 13.5, 13, 9.5, 14.5, 11)

gr2_a <- c(28.0, 38.0, 39.0, 32.0, 31.0, 27.0, 26.0, 26.0, 24.0, 23.0, 19.0, 12.0, 22.0, 16.0, 22.0, 20.0, 17.0, 27.0, 16.0, 17.0, 18.0, 15.0, 17.0, 14.0, 15.0, 11.0, 14.0, 8.0, 5.0, 4.0)
gr2_b <- c(10,5,11,4,5,6,3,3,11,12,13,10,8,9,15,24,22,24,22,16,22,23,20,24,18,16,13,14,11,14)

#1. Czy dlugosc najdluzszej osi, posredniej osi i najkrotszej osi rozni sie istotnie dla zbiorów grupy 1 i 2? (odpowiedz w oparciu o analize par zbiorow)
#2. Jaka jest korelacja miedzy dlugoscia poszczegolnych osi w zbiorze Twojej grupy?
####Odpowiedz na powyzsze pytania wykorzystujac odpowiednie metody statystyczne. Funkcje przydatne do wykonania zadan: shapiro.test(), var.test(), t.test() (zwroc uwage na ustawienie parametru var.equal!), wilcox.test(), cor(), cor.test() (zarowno przy cor, jak i przy cor test zwroc uwage na ustawienie parametru method)
