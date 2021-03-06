wydajnosc1 <- c(23.05, 21.16, 19.21, 22.19, 18.54, 27.09, 30.49, 25.74, 23.37, 
                15.49, 32.99, 23.48, 13.29, 21.36, 18.52, 26.79, 20.91, 26.93,
                24.2, 18.76, 20.6, 22.82, 24.82, 16.37, 30.07, 20.3, 22.84, 
                20.14, 17.07, 22.87, 27.31, 30.69, 24.69, 28.64)

wydajnosc2 <- c(21.41, 14.63, 16.18, 23.69, 19.16, 29.05, 25.22, 23.11, 24.85, 
                24.24, 21.32, 13.7, 22.71, 28.68, 12.58, 22.07, 36.14, 25.97, 
                14.6, 35.78, 25.11, 20.86, 27.93, 12.02, 27.17, 21.62, 16.43, 
                26.5, 23.68, 16.57, 8.16, 11.24, 25.95, 26.13, 17.96, 20.28, 29.58)



wydajnosc1_szereg_szczegolowy <- sort(wydajnosc1)
wydajnosc2_szereg_szczegolowy <- sort(wydajnosc2)

wydajnosc1tmp <- na.omit(wydajnosc1_szereg_szczegolowy)
wydajnosc2tmp <- na.omit(wydajnosc2_szereg_szczegolowy)

#histogramy rozkĹ‚adĂłw emiprycznych (rozkĹ‚ad emipryczny == szereg rozdzielczy)
count1 <- sqrt(length(wydajnosc1_szereg_szczegolowy))
count2 <- sqrt(length(wydajnosc2_szereg_szczegolowy))

breaks1 <- seq(min(wydajnosc1tmp), max(wydajnosc1tmp), length.out = count1)
breaks2 <- seq(min(wydajnosc2tmp), max(wydajnosc2tmp), length.out = count2)

wydajnosc1_szereg_rozdzielczy <- hist(wydajnosc1tmp, breaks1)
wydajnosc2_szereg_rozdzielczy <- hist(wydajnosc2tmp, breaks2)


#zad 1- miary przecietne(srednia, mediana, dominanta)
#srednia
SredniaRoz <- function(x) {
  return(sum(x$mids*x$counts)/sum(x$counts))
}

wydajnosc1.srednia_arytmetyczna_szczegolowy <- mean(wydajnosc1_szereg_szczegolowy)
wydajnosc2.srednia_arytmetyczna_szczegolowy <- mean(wydajnosc2_szereg_szczegolowy)
wydajnosc1.srednia_arytmetyczna_rozdzielczy <- SredniaRoz(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.srednia_arytmetyczna_rozdzielczy <- SredniaRoz(wydajnosc2_szereg_rozdzielczy)

# mediana
Mediana <- function(x) {
  pos <- (sum(x$counts+1))/2
  i <- findInterval(pos, cumsum(x$counts))
  return(x$breaks[i] + (pos - x$counts[i-1])*(x$breaks[i+1]-x$breaks[i])/x$counts[i])
}

wydajnosc1.mediana_szczegolowy <- median(wydajnosc1_szereg_szczegolowy)
wydajnosc2.mediana_szczegolowy <- median(wydajnosc2_szereg_szczegolowy)
wydajnosc1.mediana_rozdzielczy <- Mediana(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.mediana_rozdzielczy <- Mediana(wydajnosc2_szereg_rozdzielczy)

#dominanta(moda) 
moda <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux)); ux[tab == max(tab)]
}

modaRoz <- function(x) {
  i <- which.max(x$counts)
  return(x$breaks[i] + (x$breaks[i+1]-x$breaks[i])*(x$counts[i]-x$counts[i-1])/((x$counts[i]-x$counts[i-1])+(x$counts[i]-x$counts[i+1])))
}

wydajnosc1.moda_szczegolowy <- moda(wydajnosc1_szereg_szczegolowy)
wydajnosc2.moda_szczegolowy <- moda(wydajnosc2_szereg_szczegolowy)
wydajnosc1.moda_rozdzielczy <- modaRoz(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.moda_rozdzielczy <- modaRoz(wydajnosc2_szereg_rozdzielczy)

#kwantyl q1dn
wydajnosc1.kwantyl_q1_szczegolowy <- quantile(wydajnosc1_szereg_szczegolowy,.25)
wydajnosc2.kwantyl_q1_szczegolowy <- quantile(wydajnosc2_szereg_szczegolowy,.25)
wydajnosc1.kwantyl_q1_rozdzielczy <- quantile(wydajnosc1_szereg_rozdzielczy$mids,.25)
wydajnosc2.kwantyl_q1_rozdzielczy <- quantile(wydajnosc2_szereg_rozdzielczy$mids,.25)

#kwantyl q3
wydajnosc1.kwantyl_q3_szczegolowy <- quantile(wydajnosc1_szereg_szczegolowy,.75)
wydajnosc2.kwantyl_q3_szczegolowy <- quantile(wydajnosc2_szereg_szczegolowy,.75)
wydajnosc1.kwantyl_q3_rozdzielczy <- quantile(wydajnosc1_szereg_rozdzielczy$mids,.75)
wydajnosc2.kwantyl_q3_rozdzielczy <- quantile(wydajnosc2_szereg_rozdzielczy$mids,.75)

#odchylenie cwiartkowe
odchylenieCw <- function(x)
{
  (quantile(x,.75) - quantile(x,.25))/2
}
odchylenieCwRoz <- function(x)
{
  (quantile(x$mids,.75) - quantile(x$mids,.25))/2
}

wydajnosc1.odchylenie_cwiartkowe_szczegolowy <- odchylenieCw(wydajnosc1_szereg_szczegolowy)
wydajnosc2.odchylenie_cwiartkowe_szczegolowy <- odchylenieCw(wydajnosc2_szereg_szczegolowy)
wydajnosc1.odchylenie_cwiartkowe_rozdzielczy <- odchylenieCwRoz(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.odchylenie_cwiartkowe_rozdzielczy <- odchylenieCwRoz(wydajnosc2_szereg_rozdzielczy)

#wspolczynnik zmiennosci odchylenia cwiartkowego(pozycyjny wspolczynnik zmiennosci)
wspZmOdchyleniaCwRoz <- function(x) {
  return(odchylenieCwRoz(x)/Mediana(x))
}
wspZmOdchyleniaCw <- function(x) {
  return(odchylenieCw(x)/median(x))
}

wydajnosc1.odchylenie_cwiartkowe_szczegolowy <- wspZmOdchyleniaCw(wydajnosc1_szereg_szczegolowy)
wydajnosc2.odchylenie_cwiartkowe_szczegolowy <- wspZmOdchyleniaCw(wydajnosc2_szereg_szczegolowy)
wydajnosc1.odchylenie_cwiartkowe_rozdzielczy <- wspZmOdchyleniaCwRoz(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.odchylenie_cwiartkowe_rozdzielczy <- wspZmOdchyleniaCwRoz(wydajnosc2_szereg_rozdzielczy)


#miary zroznicowania(wariancja, odchylenie standardowe, wspolczynnik zmiennosci, rozstep)
#wariancja
wariancjaRoz <- function(x) {
  return (sum((x$mids-(SredniaRoz(x)))^2*x$counts,na.rm=TRUE)/sum(x$counts))
}


wydajnosc1.wariancja_szczegolowy <- var(wydajnosc1_szereg_szczegolowy)
wydajnosc2.wariancja_szczegolowy <- var(wydajnosc2_szereg_szczegolowy)
wydajnosc1.wariancja_rozdzielczy <- wariancjaRoz(wydajnosc1_szereg_rozdzielczy)
wydajnosc1.wariancja_rozdzielczy <- wariancjaRoz(wydajnosc2_szereg_rozdzielczy)

#odchylenie standardowe
odchylenieRoz <- function(x) {
  return (sqrt(sum((x$mids-(SredniaRoz(x)))^2*x$counts,na.rm=TRUE)/sum(x$counts)))
}

varBiasedRoz <- function(x) { #estymator obciazony wariancji rozdzielczego
  sum((x$mids - SredniaRoz(x))^2 * x$counts) / sum(x$counts);
  #mianownik rowny liczbie pomiarow n
}

varUnbiasedRoz <- function(x) { #estymator nieobciazony wariancji rozdzielczego
  sum((x$mids - SredniaRoz(x))^2 * x$counts) / (sum(x$counts) - 1);
  #mianownik rowny liczbie pomiarow n - 1
}

varBiasedSz <- function(x) { #estymator obciazony wariancji szczegolowego
  sum((x - mean(x))^ 2) / length(x); 
  #mianownik rowny liczbie pomiarow n
}

varUnbiasedSz <- function(x) { #estymator nieobciazony wariancji szczegolowego
  sum((x - mean(x))^ 2) / (length(x) - 1); 
  #mianownik rowny liczbie pomiarow n - 1
}

#odchylenie standardowe obciazone szczegolowy
sdBiasedSz <- function (x) { 
  sqrt(varBiasedSz(x));
}

#odchylenie standardowe nieobciazone szczegolowy
sdUnbiasedSz <- function (x) { 
  sqrt(varUnbiasedSz(x));
}

#odchylenie standardowe obciazone rozdzielczy
sdBiasedRoz <- function (x) { 
  sqrt(varBiasedRoz(x));
}

#odchylenie standardowe nieobciazone rozdzielczy
sdUnbiasedRoz <- function (x) { 
  sqrt(varUnbiasedRoz(x));
}

wydajnosc1.odchylenie_std_szczegolowy_obc <- sdBiasedSz(wydajnosc1_szereg_szczegolowy)
wydajnosc2.odchylenie_std_szczegolowy_obc <- sdBiasedSz(wydajnosc2_szereg_szczegolowy)
wydajnosc1.odchylenie_std_rozdzielczy_obc <- sdBiasedRoz(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.odchylenie_std_rozdzielczy_obc <- sdBiasedRoz(wydajnosc2_szereg_rozdzielczy)


wydajnosc1.odchylenie_std_szczegolowy_nieobc <- sdUnbiasedSz(wydajnosc1_szereg_szczegolowy)
wydajnosc2.odchylenie_std_szczegolowy_nieobc  <- sdUnbiasedSz(wydajnosc2_szereg_szczegolowy)
wydajnosc1.odchylenie_std_rozdzielczy_nieobc  <- sdUnbiasedRoz(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.odchylenie_std_rozdzielczy_nieobc  <- sdUnbiasedRoz(wydajnosc2_szereg_rozdzielczy)

#wspolczynnik zmiennosci
wZm <- function(x) {
  return(sd(x)/mean(x)*100)
} 
wZmRoz <- function(x) {
  return(odchylenieRoz(x)/SredniaRoz(x)*100)
}

wydajnosc1.wspl_zmiennosci_szczegolowy <- wZm(wydajnosc1_szereg_szczegolowy)
wydajnosc2.wspl_zmiennosci_szczegolowy <- wZm(wydajnosc2_szereg_szczegolowy)
wydajnosc1.wspl_zmiennosci_rozdzielczy <- wZmRoz(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.wspl_zmiennosci_rozdzielczy <- wZmRoz(wydajnosc2_szereg_rozdzielczy)

#rozstep
rozstep <- function(x) {
  return(max(x)-min(x))
}
rozstepRoz <- function(x) {
  return(max(x$mids)-min(x$mids))
}

wydajnosc1.rozstep_szczegolowy <- rozstep(wydajnosc1_szereg_szczegolowy)
wydajnosc2.rozstep_szczegolowy <- rozstep(wydajnosc2_szereg_szczegolowy)
wydajnosc1.rozstep_rozdzielczy <- rozstepRoz(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.rozstep_rozdzielczy <- rozstepRoz(wydajnosc2_szereg_rozdzielczy)

#miary koncentracji(kurtoza, skosnosc)
# to tylko daj raz zeby zainstalowalo 
#install.packages("e1071")
library(e1071)
#kurtoza
kurtozaRoz <- function (x) {
  return(moment(x$mids, order=4, center=TRUE)/(odchylenieRoz(x)^4))
}
kurtoza <- function (x) {
  moment(x, order=4, center=TRUE) / (sd(x)^ 4);
}

wydajnosc1.kurtoza_szczegolowy <- kurtosis(wydajnosc1_szereg_szczegolowy)
wydajnosc2.kurtoza_szczegolowy <- kurtosis(wydajnosc2_szereg_szczegolowy)
wydajnosc1.kurtoza_rozdzielczy <- kurtozaRoz(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.kurtoza_rozdzielczy <- kurtozaRoz(wydajnosc2_szereg_rozdzielczy)

#Eksces
Eksces <- function(x) {
  return(kurtoza(x)-3)
}

EkscesRoz <- function(x) {
  return(kurtozaRoz(x)-3)
}

wydajnosc1.eksces_szczegolowy <- Eksces(wydajnosc1_szereg_szczegolowy)
wydajnosc2.eksces_szczegolowy <- Eksces(wydajnosc2_szereg_szczegolowy)
wydajnosc1.eksces_rozdzielczy <- EkscesRoz(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.eksces_rozdzielczy <- EkscesRoz(wydajnosc2_szereg_rozdzielczy)

#skosnosc
skosnoscRoz <- function (x) {
  return((SredniaRoz(x)-modaRoz(x))/odchylenieRoz(x))
}

wydajnosc1.skosnosc_szczegolowy <- skewness(wydajnosc1_szereg_szczegolowy)
wydajnosc2.skosnosc_szczegolowy <- skewness(wydajnosc2_szereg_szczegolowy)
wydajnosc1.skosnosc_rozdzielczy <- skosnoscRoz(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.skosnosc_rozdzielczy <- skosnoscRoz(wydajnosc2_szereg_rozdzielczy)

#miary asymetrii(wspolczynnik asymetrii)
wsplAsymetrii <- function(x) {
  return(moment(x, order=3, center=TRUE)/(sd(x)^3))
}

momentRoz <- function(x, n) {
  return(sum((x$mids-SredniaRoz(x))^n*x$counts)/sum(x$counts))
}

wsplAsymetriiRoz <- function(x) {
  return(momentRoz(x,3)/(odchylenieRoz(x)^3))
}

wydajnosc1.pozycyjny_wspolczynnik_zmiennosci_szczegolowy <- wsplAsymetrii(wydajnosc1_szereg_szczegolowy)
wydajnosc2.pozycyjny_wspolczynnik_zmiennosci_szczegolowy <- wsplAsymetrii(wydajnosc2_szereg_szczegolowy)
wydajnosc1.pozycyjny_wspolczynnik_zmiennosci_rozdzielczy <- wsplAsymetriiRoz(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.pozycyjny_wspolczynnik_zmiennosci_rozdzielczy <- wsplAsymetriiRoz(wydajnosc2_szereg_rozdzielczy)

# odchylenie przecietne od sredniej
odchyleniePrzecietneSr <- function(x) {
  sum(abs(x - mean(x))) / length(x)
}

odchyleniePrzecietneRozSr <- function(x) {
  sum(abs(x$mids - SredniaRoz(x))*x$counts) / sum(x$counts)
}

wydajnosc1.odchylenie_przecietne_srednia_szczegolowy <- odchyleniePrzecietneSr(wydajnosc1_szereg_szczegolowy)
wydajnosc2.odchylenie_przecietne_srednia_szczegolowy <- odchyleniePrzecietneSr(wydajnosc2_szereg_szczegolowy)
wydajnosc1.odchylenie_przecietne_srednia_rozdzielczy <- odchyleniePrzecietneRozSr(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.odchylenie_przecietne_srednia_rozdzielczy <- odchyleniePrzecietneRozSr(wydajnosc2_szereg_rozdzielczy)

# odchylenie przecietne od mediany
odchyleniePrzecietneMe <- function(x) {
  sum(abs(x - mean(x))) / length(x)
}

odchyleniePrzecietneRozMe <- function(x) {
  sum(abs(x$mids - Mediana(x))*x$counts) / sum(x$counts)
}

wydajnosc1.odchylenie_przecietne_mediana_szczegolowy <- odchyleniePrzecietneMe(wydajnosc1_szereg_szczegolowy)
wydajnosc2.odchylenie_przecietne_mediana_szczegolowy <- odchyleniePrzecietneMe(wydajnosc2_szereg_szczegolowy)
wydajnosc1.odchylenie_przecietne_mediana_rozdzielczy <- odchyleniePrzecietneRozMe(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.odchylenie_przecietne_mediana_rozdzielczy <- odchyleniePrzecietneRozMe(wydajnosc2_szereg_rozdzielczy)

#zad 2 test Kołmogorowa lilieforsa
lilliefors <- function (x) 
{
  srednia_x <- mean(x)
  odchylenie_sd <- sd(x)
  xsort = sort(x)
  temp = (1:length(x))/length(x)
  dn <- max(abs(temp-pnorm(xsort,srednia_x,odchylenie_sd)))
  wartosc_krytyczna <- 0.886/sqrt(length(x))
  
  if(dn < wartosc_krytyczna)
  {
    print("Wartosc statystyki testowej: ")
    print(dn)
    print("Wartosc krytyczna: ")
    print(wartosc_krytyczna)
    return("Brak podstaw, by stwierdzić, że rozkład nie jest rozkładem normalnym")
  }
  else
  {
    if(dn > 1)
    {
      print("Wartosc statystyki testowej: ")
      print(dn)
      print("Wartosc krytyczna: ")
      print(wartosc_krytyczna)
      return("Brak podstaw, by stwierdzić, że rozkład nie jest rozkładem normalnym")
    }
    else
    {
      print("Wartosc statystyki testowej: ")
      print(dn)
      print("Wartosc krytyczna: ")
      print(wartosc_krytyczna)
      return("Hipoteza odrzucona - rozkład nie jest rozkładem normalnym")    
    }
    
  }
}

lilliefors(wydajnosc1)
lilliefors(wydajnosc2)
wydajnosc1_kolm <- lilliefors(wydajnosc1)
wydajnosc2_kolm <- lilliefors(wydajnosc2)


#Zad. 3 Skrypt str 117
test_zad_3 <- function(dane, m0)
{
  a <- 0.05
  T1 <- (mean(dane) - m0) /  sd(dane) * sqrt(length(dane) - 1)
  t <- qt(1 - a/2, length(dane) - 1) 
  print ("Wartosc statystyki testowej wynosi: ")
  print(T1)
  print ("Kwantyl rozkladu t-studenta ")
  print(t)
  if (T1 < t && T1 > -t) {
    print("Brak podstaw do odrzucenia hipotezy dotyczacej średniej")
  } else {
    print("Są podstawy do odrzucenia hipotezy  dotyczacej średniej")
  }
}

test_zad_3(wydajnosc1, 22)

#Zad. 4 Skrypt str 119
test_zad_4 <- function(dane, odchylenie_std)
{
  wariancja <- odchylenie_std ^ 2
  a <- 0.05
  
  X1_kw <- length(dane) * sd(dane) ^ 2 / wariancja
  chi_1 <- qchisq(a/2, length(dane) - 1)
  chi_2 <- qchisq(1 - a/2, length(dane) - 1)
  print ("Wartosc statystyki testowej wynosi: ")
  print(X1_kw)
  print ("Kwantyl rozkladu chi-kwadrat a/2 ")
  print(chi_1)
  print ("Kwantyl rozkladu chi-kwadrat 1 - a/2 ")
  print(chi_2)
  if (X1_kw < chi_2 && X1_kw > chi_1) {
    print("Brak podstaw do odrzucenia hipotezy dotyczacej odchylenia standardowego")
  } else {
    print("Sa podstawy do odrzucenia hipotezy dotyczacej odchylenia standardowego")
  }
}

test_zad_4(wydajnosc2, 7)

#install.packages("nortest")
library("nortest")

przecietna_wydajnosc <- function(x)
{
  t <- ((mean(x) - 22)/sd(x))*sqrt(length(x))
  t <- t*0.34
}
czy_wieksza_wydajnosc <- function(x1, x2)
{
  u <- mean(x1) - mean(x2)
  m <- (sd(x1)*sd(x1)/length(x1)) +  (sd(x2)*sd(x2)/length(x2))
  wynik <- u/sqrt(m)
}
tmp <- czy_wieksza_wydajnosc(wydajnosc1, wydajnosc2)


odchylenie_std_wydajnosci <- function(x)
{
  u <- length(x) * sd(x)*sd(x)
  l <- u/54.437
  r <- u/21.336
}

test_zad_5 <- function(wydajnosc1, wydajnosc2)
{
  roznicaWariancji <- abs(var(wydajnosc1) - var(wydajnosc2))
  roznicaSrednich <- abs(mean(wydajnosc1) - mean(wydajnosc2))
  
  WartoscStatystykiTestowej <- (mean(wydajnosc1) - mean(wydajnosc2))
  
  if(roznicaSrednich <= roznicaWariancji)
  {
    WartoscStatystykiTestowej <- WartoscStatystykiTestowej/(sqrt(((odchylenie_std_wydajnosci(wydajnosc1)*odchylenie_std_wydajnosci(wydajnosc1))/length(wydajnosc1))
                                                                 +((odchylenie_std_wydajnosci(wydajnosc2)*odchylenie_std_wydajnosci(wydajnosc2))/length(wydajnosc2)))) 
  }
  else
  {
    WartoscStatystykiTestowej <- WartoscStatystykiTestowej/sqrt((length(wydajnosc1)*var(wydajnosc1)*var(wydajnosc1) + length(wydajnosc2)*var(wydajnosc2)*var(wydajnosc2))/(length(wydajnosc1)+length(wydajnosc2)-2)*((1/length(wydajnosc1))*1/length(wydajnosc2)))
  }
  prawostronnyObszarKrytyczny <- 1.96
  
  print ("Wartosc statystyki testowej wynosi: ")
  print (WartoscStatystykiTestowej)
  
  if (WartoscStatystykiTestowej > prawostronnyObszarKrytyczny)
  {
    print ("Można przyjąć, iż wartości wydajności pracy na starej hali są większe")
  }
  else
  {
    print ("Nie można powiedzieć, aby wartości wydajności pracy na starej hali były większe")
  }
  
}

test_zad_5(wydajnosc1, wydajnosc2)
