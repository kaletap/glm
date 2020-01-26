########## Przemysław Kaleta - kolokwium GLM #########
library(dplyr)

data <- read.table("z1.txt", header=TRUE)
data

###### Zadanie 1

### a) Wykresy odpowiedzi w zależności od wieku
make_plot <- function(s, ti) {
  grouped <- data %>% filter(sex == s, qual == 1) %>% select(age, y)
  plot(as.numeric(grouped$age)*20, grouped$y, type="l", col=1, ylim=c(0, 50), xlab="age", ylab="count", lty=1)
  title(ti)
  grouped <- data %>% filter(sex == s, qual == 2) %>% select(age, y)
  lines(as.numeric(grouped$age)*20, grouped$y, type="l", col=2, lty=2)
  grouped <- data %>% filter(sex == s, qual == 3) %>% select(age, y)
  lines(as.numeric(grouped$age)*20, grouped$y, type="l", col=3, lty=3)
  legend(50, 50, c("low", "med", "high"), lty=1:3, col=c(1, 2, 3), cex=0.8)
}

make_plot(0, "Women")
make_plot(1, "Men")

#### b) 
## Model wielomianowy (nie uwzględniamy że qual jest zmienną porządkową) (lab 7)
library(tidyr)
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
