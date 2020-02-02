library(docstring)

data <- read.csv("drzewa.txt", sep=" ")

get_l_gradient <- function(beta, gamma, x, y) {
  n <- length(gamma)
  stopifnot(nrow(x) == n, nrow(y) == n)
  l_gradient_beta_0 <- sum(y / (gamma * exp(beta[1] + beta[2]*x)) - 1)
  l_gradient_beta_1 <- sum(y * x / (gamma * exp(beta[1] + beta[2]*x)) - x)
  rbind(l_gradient_beta_0, l_gradient_beta_1)
}

get_fisher_matrix <- function(x) {
  n <- length(x)
  x_sum <- sum(x)
  x_squared_sum <- sum(x^2)
  rbind(c(x_squared_sum, -x_sum), c(-x_sum, n)) / (n*x_squared_sum - x_sum^2)
}


fit_glm <- function(data, y_name, gamma_name, x_name, eps=1e-3, n_iter=1000) {
  #' Fits generalized linear model (GLM) defined as E[y|x] = gamma exp(beta_0 + beta_1*x), y ~ Exp(lambda) 
  #' using Fisher Scoring
  #' 
  #' Arguments:
  #' @data data frame with observations
  #' @y_name, gamma_name, x_name: names of the variables in data
  #' @eps tolerance of the method after which iteration stops 
  #' (given the distance between new and old beta vector)
  #' @n_iter number of iteration after which iteration stops
  #' 
  #' Details:
  #' This function performs iterations of beta coefficients using Fisher Scoring method.
  #' Since gradients sometimes explode (as observed empirically), when it happens the iteration is restarted with
  #' other random value of beta - generated from standard normal distribution. This is the reason that 
  #' this function may take suprisingly many iterations.
  #' 
  #' Value:
  #' @beta two dimensional vector of fitted coefficients in the model

  # Prepare data
  y <- as.numeric(data[[y_name]])
  gamma <- as.numeric(data[[gamma_name]])
  x <- as.numeric(data[[x_name]])
  
  fisher_matrix <- get_fisher_matrix(x)
  
  # Fisher scoring method iteration
  initial_beta <- rbind(rnorm(1), rnorm(1))
  new_random <- FALSE # boolean value if beta was just regenerated (it happens after beta becomes NA)
  beta <- initial_beta
  for (i in 1:n_iter) {
    beta_old <- beta
    beta <- beta + fisher_matrix %*% get_l_gradient(beta, gamma, x, y)
    if (any(is.na(beta))) {  # this may happen if beta becomes too large
      beta <- rbind(rnorm(1), rnorm(1))
      new_random <- TRUE
    } else {
      error <- mean((beta- beta_old)^2)
      if (!new_random && error < eps){
        print(paste("Converged at iteration number", i))
        return(beta)
      }
      new_random <- FALSE
    }
  }
  warning("Beta may have not converged. Consider running this function again.")
  beta
}

fit_glm(data, "y", "ga", "x", eps=1e-5)
# Nasza metoda zwraca wyniki beta_0 = -1.47, beta_1 = 0.03

model <- glm(y/ga ~ x, family=Gamma(link="log"), data=data)
summary(model, dispersion=1)

# Wynik wbudowanej metody glm okazuje się taki sam (-1.47 oraz 0.03)

# W metodzie glm wykorzystujemy rozkład gamma z logarytmiczną funkcją łączącą.
# Ponieważ postać modelu nie ma w sobie współczynnika przy zmiennej gamma, to obliczamy 
# współczynniki traktując zmienną odpowiedzi jako y / gamma. Innymi słowy, model który dopasowujemy to
# z = exp(beta_0 + beta_1*x). Moglibyśmy to też zrobić za pomocą offsetu (współczynnik przy gamma jest stały).

