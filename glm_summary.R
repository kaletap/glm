### Przykład użycia GLM dla danych zgrupowanych
library(faraway)
data(bliss)
bliss
y <- as.matrix(bliss[, c("dead", "alive")])  # y jest macierzą zliczeń tak/nie (umarł/nie umarł)
glm(y ~ conc, data = bliss, family=binomial)

### Model wielomianowy ~regresja logistyczna, tylko dla wielu zmiennych odpowiedzi
library(MASS)
head(VA) # dane Veteran's Administration Lung Cancer Trial
VA$cell # typ komórki (możliwe 1, 2, 3, 4)
library(nnet)
multinom(cell ~ age + treat, data=VA)

### Model proporcjonalnych sznas
# polecenie z labów: http://pages.mini.pw.edu.pl/~piliszeka/old/admin/glm_lab/GLMlab08.pdf
# data: http://pages.mini.pw.edu.pl/~piliszeka/old/admin/glm_lab/impair.data
impair <- read.table("impair.data.txt", header=TRUE)
head(impair)
library(MASS)
impair$mental <- as.ordered(impair$mental)
impair$mental
model_prop <- polr(mental ~ ses + events, data=impair)
summary(model_prop)

# Interpretacja: https://data.library.virginia.edu/fitting-and-interpreting-a-proportional-odds-model/
# Pstwo, ze czlowiek majacy niski (0) status socjo-ekonomiczny i 6 zdarzeń życiowych będzie miał umiarkowany (3) albo
# gorszy (4) stan zdrowia psychicznego
probability_of_crazy <- function(ses_status, n_events) {
  # logit(P(Y<=2)) = intercept_{2|3} - beta_ses*ses - beta_events*events
  intercept <- 1.2127926
  logit <- intercept - (-1.1112310)*ses_status - 0.3188613*n_events
  p_2 <- 1 / (1 + exp(logit))
  1 - p_2
}
probability_of_crazy(0, 6)
# Czyli szukane pstwo to 0.33

# Zwróćmy uwagę, że model ma sens: współczynnik beta przy events jest dodatni ->
# poniewaz w modelu beta jest z minusem oznacza to, że im więcej zdarzeń, tym pstwo, że mamy doby stan umysłu
# maleje. Z kolei ze statusem socjoekonomicznym jest odwrotnie - im większy (lepszy) tym większa szansa na 
# zachowanie dobrego stanu umysłu.
