library(tidyverse)
library(tsibble)

#library(feast)

cetesb_pinheiros_co <- readRDS("dados/cetesb_pinheiros_diario_co.rds") |>
  mutate(
    mes = lubridate::month(data),
    Mes = lubridate::floor_date(data, "month")
  )

cetesb_pinheiros_co

cetesb_pinheiros_co_ts <- as_tsibble(
  cetesb_pinheiros_co, index = data)

cetesb_pinheiros_co_ts

# DIFERENÇAS

library(feasts)

# não funciona
cetesb_pinheiros_co |>
  autoplot()

# funciona
cetesb_pinheiros_co_ts |>
  autoplot()

# outra coisa que funciona, e ajuda a cortar caminho
cetesb_pinheiros_co_ts |>
  gg_season()

# até seria possível fazer de outro jeito, mas o CONTEXTO
# permite que os pacotes já se preparem para
# nos dar o que a gente precisa

# séries temporais apresentam gráficos de lag com uma cara bem típica:

cetesb_pinheiros_co_ts |>
  gg_lag(geom = "point", lag = 1:4)

cetesb_pinheiros_co_ts |>
  gg_lag(geom = "point", lag = 1:9)

# já dados transversais costumem não fazer desenhos típicos de variáveis associadas

# Exemplo da base USArrests

USArrests |>
  mutate(id = 1:n()) |>
  as_tsibble(index = id) |>
  gg_lag(geom = "point", lag = 1:4)

# Como muitas vezes os gráficos dos lags tem
# tendências lineares, existe um outro gráfico
# interessanteque se chama gráfico da autocorrelação.

cetesb_pinheiros_co_ts |>
  ACF(concentracao) |>
  autoplot()

# cada linha vertical desse gráfico é a correlação
# linear de Pearson entre x_lag e x. Frequentemente
# esse gráfico nos informa coisas importantes sobre a
# dinâmica da série temporal. Nesse caso, temos
# aparentemente ciclos de 10 dias acontecendo

# gráficos de análise sazonal

## gg_season

cetesb_pinheiros_co_ts |>
  gg_season()

cetesb_pinheiros_co_ts |>
  gg_season(polar = TRUE)

cetesb_pinheiros_co_ts |>
  gg_season(period = 7)

cetesb_pinheiros_co_ts |>
  gg_season(period = 7, polar = TRUE)

## gg_subseries

cetesb_pinheiros_co_ts |>
  gg_subseries(period = 7)

# Tudo que vimos aqui se aplica também a grupos:

cetesb_pinheiros <- readRDS("dados/cetesb_pinheiros_diario.rds")

cetesb_pinheiros_ts <- as_tsibble(
  cetesb_pinheiros, index = data)

cetesb_pinheiros_ts <- as_tsibble(
  cetesb_pinheiros, index = data, key = poluente)

cetesb_pinheiros_ts |>
  autoplot()

cetesb_pinheiros_ts |>
  gg_season()

cetesb_pinheiros_ts %>%
  gg_subseries(period = 7)

cetesb_pinheiros_ts |>
  ACF() |>
  autoplot()

# STL

cetesb_pinheiros_ts |>
  tsibble::fill_gaps() |>
  model(
    STL(
      concentracao ~ season(window = "peridic")
    )) |>
  autoplot()


