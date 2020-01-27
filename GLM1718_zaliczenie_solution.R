########## Przemysław Kaleta - kolokwium GLM #########
library(tidyr)
library(dplyr)

data <- read.table("z1.txt", header=TRUE)
data

###### Zadanie 1
### a) Wykresy odpowiedzi w zależności od wieku
make_plot <- function(s, ti) {
  grouped <- data %>% filter(sex == s, qual == 1) %>% dplyr::select("age", "y")
  plot(as.numeric(grouped$age)*20, grouped$y, type="l", col=1, ylim=c(0, 50), xlab="age", ylab="count", lty=1)
  title(ti)
  grouped <- data %>% filter(sex == s, qual == 2) %>% dplyr::select("age", "y")
  lines(as.numeric(grouped$age)*20, grouped$y, type="l", col=2, lty=2)
  grouped <- data %>% filter(sex == s, qual == 3) %>% dplyr::select("age", "y")
  lines(as.numeric(grouped$age)*20, grouped$y, type="l", col=3, lty=3)
  legend(50, 50, c("low", "med", "high"), lty=1:3, col=c(1, 2, 3), cex=0.8)
}

make_plot(0, "Women")
make_plot(1, "Men")

#### b) 
## Model wielomianowy (nie uwzględniamy że qual jest zmienną porządkową) (lab 7)
data_grouped <- data %>% spread(qual, y, sep="_")
library(nnet)
y <- as.matrix(data_grouped[, c("qual_1", "qual_2", "qual_3")])
modwiel <- multinom(y ~ age + sex, data=data_grouped)
summary(modwiel)

## Model proporcjonalnych szans (uwzględniamy uporządkowanie w zmiennej qual)
# Lab 8: http://pages.mini.pw.edu.pl/~piliszeka/old/admin/glm_lab/GLMlab08.pdf
data_unrolled <- data[rep(1:nrow(data), data$y), c("sex", "age", "qual")]
data_unrolled$qual <- as.ordered(data_unrolled$qual)
head(data_unrolled)

library(MASS)
library(dplyr)
modpropsz <- polr(qual ~ sex + age, data=data_unrolled)
summary(modpropsz)

## Jakość dopasowania (dewiancja)
null_modwiel <- multinom(y ~ 1, data=data_grouped)
null_modpropsz <- polr(qual ~ 1, data=data_unrolled)
goodness_of_fit <- function(model, null_model) {
  1 - deviance(model)/ deviance(null_model)
}
goodness_of_fit(modwiel, null_modwiel)
goodness_of_fit(modpropsz, null_modpropsz)

# Hmm, dopasowanie modelu proporcjonalnych szans jest gorsze

### c)
### d)


#### Zadanie 2
# Dane osób palących papierosy i mających podwyższone ciśnienie
df <- data.frame(plec=c("woman", "woman", "woman","woman", "man", "man", "man", "man"),
                 lek=c(1, 1, 0, 0, 1, 1, 0, 0),
                 pal=c(1, 0, 1, 0, 1, 0, 1, 0),
                 count=c(14, 12, 7, 25, 2, 8, 22, 84))

tab <- xtabs(count ~ lek + pal + plec, df)
tab
# Szacowane pstwa że ktoś ma nadciśnienie (przyjmuje leki na nadciśnienie)
# pod warunkiem tego że pali/nie pali
# dla mezczyczn p(lek | pali) = 0.08, p(lek | nie pali) = 0.09
# dla kobiet    p(lek | pali) = 0.32, p(lek | nie pali) = 0.57

# Czyli u meżczyzn osoby palące mają trochę mniejsze prawdopodobieństwo, że przyjmują leki na nadciśnienie.
# Z kolei palące kobiety mają dużo większe pstwo, że przyjmują leki na nadciśnienie w porównaniu z niepalącymi.
# Uwaga: 1. to, że ktoś nie przyjmuje lekóW na nadciśnienie nie znaczy że go nie ma.
#        2. nie wiemy czy wyniki te są istotne statystycznie

# Rozważmy najpierw tylko zbiór kobiet i zastanówmy się czy możemy coś powiedzieć o tej zależności
df_woman <- df %>% filter(plec == "woman")  %>% spread(lek, count, sep="_")
y <- as.matrix(df_woman %>% dplyr::select(lek_0, lek_1))
model_binomial <- glm(y ~ pal, family=binomial, data=df_woman)
summary(model_binomial)
# wygląda na to, że zmienna pal istotnie wpływa na to, czy kobieta bierze leki na nadciśnienie (p_val=0.014)

# Zróbmy to samo dla całej populacji:
df_all <- df %>% spread(lek, count, sep="_")
y <- as.matrix(df_all %>% dplyr::select(lek_0, lek_1))
model_binomial <- glm(y ~ pal, family=binomial, data=df_all)
summary(model_binomial)
# Zmienna pal jest istotna (jescze bardziej), chociaż jej wielkość teraz to tylko -1.1 (zamiast -1.4).
# W każdym razie, wygląda na to, że zmienna pal istotnie wpływa na szanse, że ktoś bierze leki na
# nadciśnienie (jeżeli jest kobietą). Raczej nie o to chodziło w tym poleceniu, ale warto wiedziec.

## a i b)
goodness_of_fit <- function(model, null_model) {
  1 - deviance(model)/ deviance(null_model)
}

model_null_poiss <- glm(count ~ 1, family=poisson, data=df)

# zbadamy niezaleznosci zmiennych
df
model_poiss <- glm(count ~ plec + lek + pal, family=poisson, data=df)
summary(model_poiss)
goodness_of_fit(model_poiss, model_null_poiss)

# zależność lek-pal (jeżeli ktoś pali to widzieliśmy trochę większą szanse, że się będzie leczył)
model_poiss2 <- glm(count ~ plec + lek * pal, family=poisson, data=df)
summary(model_poiss2)
anova(model_poiss, model_poiss2, test="Chi")
# p_value = 0.005, czyli model z interakcją między leczeniem i paleniem jest istotnie 
# lepszy niż model bez tej interakcji. Wynika z tego, że jakaś zależność jest.
goodness_of_fit(model_poiss2, model_null_poiss)

# zaleznosc - plec-lek (no widać było że więcej kobiet się leczy)
model_poiss3 <- glm(count ~ plec * lek + pal, family=poisson, data=df)
summary(model_poiss3)
anova(model_poiss, model_poiss3, test="Chi")  # najmniejsza wartość testu Chi
goodness_of_fit(model_poiss3, model_null_poiss)  # najmniejsza dewiancja! najlepiej dopasownany model

# zależność płeć - palenie
model_poiss4 <- glm(count ~ lek + plec * pal, family=poisson, data=df)
summary(model_poiss4)
anova(model_poiss, model_poiss4, test="Chi")  # jakaś zależność jest, ale dużo mniejsza
goodness_of_fit(model_poiss4, model_null_poiss)

## c)
# warunkowa zależność płeć-palenie
xtabs(count ~ plec + pal + lek, df)
# z tabelek widać, że jeżeli ustalimy zmienną lek to nie ma zbyt dużej zależności między
# płcią a tym czy ktoś pali dla nie leczących się osób (proporcje są podobne).
# dla osób leczących się próbka jest mniejsza, widać że kobiety palą trochę więcej, ale nie jesteśmy
# pewni czy jest to istotne statystycznie. Sprawdzimy czy dodanie kolejnej interakcji wyjaśni ten model
# (w stosunku do pojedynczej zaleznosci plec-lek)
model_poiss5 <- glm(count ~ plec*pal + plec*lek, family=poisson, data=df)
summary(model_poiss5)
anova(model_poiss3, model_poiss5, test="Chi")
# p-value jest poniżej 0.05, ale dość duże. Możliwe, że jest zależność warunkowa.

# Podsumowując: każdy model z interakcjami był istotnie lepszy od modelu bez.
# Najlepiej dopasowany (pod względem Chi^2 oraz dewiancji) był model z interakcją płeć-lek,
# zależność tę widać od razu z tabeli.

## d) klasyfikacja ze wzgledu na plec i stosowanie leków
marginal_df <- df %>% group_by(lek, plec) %>% summarize(count = sum(count))
# brzegowa tablica kontyngencji klasyfikująca ze względu na płeć i stosowanie leków
tab <- xtabs(count ~ lek + plec, marginal_df)
# Interpretacja: tak jak pokazywałem na samym początku kobiety znacznie częściej się leczą w stosunku do
# mężczyzn - niezależnie od tego czy palą czy nie (32 na 58 kobiet oraz tylko 10 na 116 mężczyzn się leczy)
# Te obserwację potwierdziliśmy też modelem poissona i testami statystycznymi.

# Pytanie: Po co te wszystkie testy? Nie wystarczy popatrzeć na tabelkę?

