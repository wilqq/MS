wydajnosc1 <- c(23.05, 21.16, 19.21, 22.19, 18.54, 27.09, 30.49, 25.74, 23.37, 15.49, 32.99, 23.48, 13.29, 21.36, 18.52, 26.79, 20.91, 26.93, 24.2, 18.76, 20.6, 22.82, 24.82, 16.37, 30.07, 20.3, 22.84, 20.14, 17.07, 22.87, 27.31, 30.69, 24.69, 28.64)
wydajnosc2 <- c(21.41, 14.63, 16.18, 23.69, 19.16, 29.05, 25.22, 23.11, 24.85, 24.24, 21.32, 13.7, 22.71, 28.68, 12.58, 22.07, 36.14, 25.97, 14.6, 35.78, 25.11, 20.86, 27.93, 12.02, 27.17, 21.62, 16.43, 26.5, 23.68, 16.57, 8.16, 11.24, 25.95, 26.13, 17.96, 20.28, 29.58)

wydajnosc1_szereg_szczegolowy <- sort(wydajnosc1)
wydajnosc2_szereg_szczegolowy <- sort(wydajnosc2)

wydajnosc1_szereg_rozdzielczy <- table(wydajnosc1)
wydajnosc2_szereg_rozdzielczy <- table(wydajnosc2)

#zad 1- miary przecietne(srednia, mediana, dominanta)
#+ kwartyle q1 i q3
#srednia
wydajnosc1.srednia_arytmetyczna_szczegolowy <- mean(wydajnosc1_szereg_szczegolowy)
wydajnosc2.srednia_arytmetyczna_szczegolowy <- mean(wydajnosc2_szereg_szczegolowy)
wydajnosc1.srednia_arytmetyczna_rozdzielczy <- mean(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.srednia_arytmetyczna_rozdzielczy <- mean(wydajnosc2_szereg_rozdzielczy)
# mediana
wydajnosc1.mediana_szczegolowy <- median(wydajnosc1_szereg_szczegolowy)
wydajnosc2.mediana_szczegolowy <- median(wydajnosc2_szereg_szczegolowy)
wydajnosc1.mediana_rozdzielczy <- median(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.mediana_rozdzielczy <- median(wydajnosc2_szereg_rozdzielczy)
# dominanta(moda) 
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}  
wydajnosc1.moda_szczegolowy <- moda(wydajnosc1_szereg_szczegolowy)
wydajnosc2.moda_szczegolowy <- moda(wydajnosc2_szereg_szczegolowy)
wydajnosc1.moda_rozdzielczy <- moda(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.moda_rozdzielczy <- moda(wydajnosc2_szereg_rozdzielczy)

# kwantyl q1
wydajnosc1.kwantyl_q1_szczegolowy <- quantile(wydajnosc1_szereg_szczegolowy,.25)
wydajnosc2.kwantyl_q1_szczegolowy <- quantile(wydajnosc2_szereg_szczegolowy,.25)
wydajnosc1.kwantyl_q1_rozdzielczy <- quantile(wydajnosc1_szereg_rozdzielczy,.25)
wydajnosc2.kwantyl_q1_rozdzielczy <- quantile(wydajnosc2_szereg_rozdzielczy,.25)

# kwantyl q3
wydajnosc1.kwantyl_q3_szczegolowy <- quantile(wydajnosc1_szereg_szczegolowy,.75)
wydajnosc2.kwantyl_q3_szczegolowy <- quantile(wydajnosc2_szereg_szczegolowy,.75)
wydajnosc1.kwantyl_q3_rozdzielczy <- quantile(wydajnosc1_szereg_rozdzielczy,.75)
wydajnosc2.kwantyl_q3_rozdzielczy <- quantile(wydajnosc2_szereg_rozdzielczy,.75)

#miary zroznicowania(wariancja, odchylenie standardowe, wspolczynnik zmiennosci)
#wariancja
wydajnosc1.wariancja_szczegolowy <- var(wydajnosc1_szereg_szczegolowy)
wydajnosc2.wariancja_szczegolowy <- var(wydajnosc2_szereg_szczegolowy)
wydajnosc1.wariancja_rozdzielczy <- var(wydajnosc1_szereg_rozdzielczy)
wydajnosc1.wariancja_rozdzielczy <- var(wydajnosc2_szereg_rozdzielczy)
#odchylenie standardowe
wydajnosc1.odchylenie_std_szczegolowy <- sd(wydajnosc1_szereg_szczegolowy)
wydajnosc2.odchylenie_std_szczegolowy <- sd(wydajnosc2_szereg_szczegolowy)
wydajnosc1.odchylenie_std_rozdzielczy <- sd(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.odchylenie_std_rozdzielczy <- sd(wydajnosc2_szereg_rozdzielczy)
#wspolczynnik zmiennosci
wZm <- function(x) {
  return(sd(x)/mean(x)*100)
} 
wydajnosc1.wspl_zmiennosci_szczegolowy <- wZm(wydajnosc1_szereg_szczegolowy)
wydajnosc2.wspl_zmiennosci_szczegolowy <- wZm(wydajnosc2_szereg_szczegolowy)
wydajnosc1.wspl_zmiennosci_rozdzielczy <- wZm(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.wspl_zmiennosci_rozdzielczy <- wZm(wydajnosc2_szereg_rozdzielczy)

#miary koncentracji(kurtoza, skosnosc)
# to tylko daj raz zeby zainstalowalo install.packages("e1071")
library(e1071)
#kurtoza
wydajnosc1.kurtoza_szczegolowy <- kurtosis(wydajnosc1_szereg_szczegolowy)
wydajnosc2.kurtoza_szczegolowy <- kurtosis(wydajnosc2_szereg_szczegolowy)
wydajnosc1.kurtoza_rozdzielczy <- kurtosis(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.kurtoza_rozdzielczy <- kurtosis(wydajnosc2_szereg_rozdzielczy)
#skosnosc
wydajnosc1.skosnosc_szczegolowy <- skewness(wydajnosc1_szereg_szczegolowy)
wydajnosc2.skosnosc_szczegolowy <- skewness(wydajnosc2_szereg_szczegolowy)
wydajnosc1.skosnosc_rozdzielczy <- skewness(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.skosnosc_rozdzielczy <- skewness(wydajnosc2_szereg_rozdzielczy)

#miary asymetrii(wspolczynnik asymetrii)
asymetria <- function(x)
{
  (mean(x) - mode(x))/sd(x)
}
wydajnosc1.asymetria_szczegolowy <- asymetria(wydajnosc1_szereg_szczegolowy)
wydajnosc2.asymetria_szczegolowy <- asymetria(wydajnosc2_szereg_szczegolowy)
wydajnosc1.asymetria_rozdzielczy <- asymetria(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.asymetria_rozdzielczy <- asymetria(wydajnosc2_szereg_rozdzielczy)

#odchylenie ćwiartkowe
odch_cw <- function(x)
{
  (quantile(x,.75) - quantile(x,.25))/2
}

wydajnosc1.odchylenie_cwiartkowe_szczegolowy <- odch_cw(wydajnosc1_szereg_szczegolowy)
wydajnosc2.odchylenie_cwiartkowe_szczegolowy <- odch_cw(wydajnosc2_szereg_szczegolowy)
wydajnosc1.odchylenie_cwiartkowe_rozdzielczy <- odch_cw(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.odchylenie_cwiartkowe_rozdzielczy <- odch_cw(wydajnosc2_szereg_rozdzielczy)

# odchylenie przecietne
odch_przecietne <- function(x)
{
  sum(abs(x - mean(x))) / length(x)
}
wydajnosc1.odchylenie_przecietne_szczegolowy <- odch_przecietne(wydajnosc1_szereg_szczegolowy)
wydajnosc2.odchylenie_przecietne_szczegolowy <- odch_przecietne(wydajnosc2_szereg_szczegolowy)
wydajnosc1.odchylenie_przecietne_rozdzielczy <- odch_przecietne(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.odchylenie_przecietne_rozdzielczy <- odch_przecietne(wydajnosc2_szereg_rozdzielczy)

#pozycyjny współczynnik zmienności
poz_wspl_zmien <- function(x)
{
  odch_cw(x) / median(x)
}
wydajnosc1.pozycyjny_wspolczynnik_zmiennosci_szczegolowy <- poz_wspl_zmien(wydajnosc1_szereg_szczegolowy)
wydajnosc2.pozycyjny_wspolczynnik_zmiennosci_szczegolowy <- poz_wspl_zmien(wydajnosc2_szereg_szczegolowy)
wydajnosc1.pozycyjny_wspolczynnik_zmiennosci_rozdzielczy <- poz_wspl_zmien(wydajnosc1_szereg_rozdzielczy)
wydajnosc2.pozycyjny_wspolczynnik_zmiennosci_rozdzielczy <- poz_wspl_zmien(wydajnosc2_szereg_rozdzielczy)

#historgarmy 
hist(wydajnosc1)
hist(wydajnosc2)

#Zad. 3 Skrypt str 117
# PYTANIE czy w obliczaniu T1 ma być pierwiastek n czy n - 1
test_zad_3 <- function(dane, m0)
{
  a <- 0.05
  T1 <- (mean(dane) - m0) /  sd(dane) * sqrt(length(dane) - 1)
  t <- qt(1 - a/2, length(dane))  
  if (T1 < t && T1 > -t) {
    print("Brak podstaw do odrzucenia hipotezy dotyczącej średniej")
  } else {
    print("Są podstawy do odrzucenia hipotezy  dotyczącej średniej")
  }
}

test_zad_3(wydajnosc1, 22)

#Zad. 4 Skrypt str 119
# PYTANIE, czy to można liczyć dla wariancji?
test_zad_4 <- function(dane, odchylenie_std)
{
  wariancja <- odchylenie_std ^ 2
  a <- 0.05
  
  X1_kw <- length(dane) * sd(dane) ^ 2 / wariancja
  chi_1 <- qchisq(a/2, length(dane) - 1)
  chi_2 <- qchisq(1 - a/2, length(dane) - 1)
  if (X1_kw < chi_2 && X1_kw > chi_1) {
    print("Brak podstaw do odrzucenia hipotezy dotyczącej odchylenia standardowego")
  } else {
    print("Są podstawy do odrzucenia hipotezy dotyczącej odchylenia standardowego")
  }
}

test_zad_4(wydajnosc2, 7)

install.packages("nortest")
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

lilliefors <- function (x) 
{
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    n <- length(x)
    if (n < 5) 
      stop("Przykladowa probka musi byc wieksza od 4")
    p <- pnorm((x - mean(x))/sd(x))
    Dplus <- max(seq(1:n)/n - p)
    Dminus <- max(p - (seq(1:n) - 1)/n)
    K <- max(Dplus, Dminus)
    if (n <= 100) {
      Kd <- K
      nd <- n
    }
    else {
      Kd <- K * ((n/100)^0.49)
      nd <- 100
    }
    pvalue <- exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 * 
                    Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598/sqrt(nd) + 
                    1.67997/nd)
    if (pvalue > 0.05) {
      KK <- (sqrt(n) - 0.01 + 0.85/sqrt(n)) * K
      if (KK <= 0.302) {
        pvalue <- 1
      }
      else if (KK <= 0.5) {
        pvalue <- 2.76773 - 19.828315 * KK + 80.709644 * 
          KK^2 - 138.55152 * KK^3 + 81.218052 * KK^4
      }
      else if (KK <= 0.9) {
        pvalue <- -4.901232 + 40.662806 * KK - 97.490286 * 
          KK^2 + 94.029866 * KK^3 - 32.355711 * KK^4
      }
      else if (KK <= 1.31) {
        pvalue <- 6.198765 - 19.558097 * KK + 23.186922 * 
          KK^2 - 12.234627 * KK^3 + 2.423045 * KK^4
      }
      else {
        pvalue <- 0
      }
    }
    RVAL <- list(statistic = c(D = K), p.value = pvalue, method = "Test Kolmogorova-Lillieforsa", 
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}

lilliefors(wydajnosc1)
lilliefors(wydajnosc2)
wydajnosc1_kolm <- lilliefors(wydajnosc1)
wydajnosc2_kolm <- lilliefors(wydajnosc2)

odchylenie_std_wydajnosci <- function(x)
{
  u <- length(x) * sd(x)*sd(x)
  l <- u/54.437
  r <- u/21.336
}

