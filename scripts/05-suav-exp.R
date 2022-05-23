# Suavização exponencial simples
library(tidymodels)

soja <- readr::read_rds("https://github.com/curso-r/main-series/raw/main/dados/soja.rds")

model <- forecast::ses(soja$KG_LIQUIDO/1000, initial = "simple")
model$model$par

sigmoid <- function(x) {
  1/(1 + exp(-x))
}

suav_exp <- function(y) {
  function(pars) {
    alpha <- sigmoid(pars[1])
    l_0 <- pars[2]
    y_hat <- numeric(length = length(y))
    y_hat[1] <- l_0
    for (i in 2:length(y)) {
      y_hat[i] <- alpha*y[i-1] + (1-alpha)*y_hat[i-1]
    }
    mean((y-y_hat)^2)
  }
}

y <- soja$KG_LIQUIDO/1000
opt <- optim(fn = suav_exp(y), par = c(0, y[1]))
sigmoid(opt$par[1])
opt$par[2]

