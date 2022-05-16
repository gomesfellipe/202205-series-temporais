
library(tidyverse)
library(tsibble)

cetesb_pinheiros_co <- readRDS("dados/cetesb_pinheiros_diario_co.rds") |>
  mutate(
    Mes = lubridate::floor_date(data, "month")
  )

cetesb_pinheiros

cetesb_pinheiros_ts <- as_tsibble(
  cetesb_pinheiros, index = data)

cetesb_pinheiros_ts

# DIFERENÇAS

library(feasts)


# não funciona
cetesb_pinheiros |>
  autoplot()

# funciona
cetesb_pinheiros_ts |>
  autoplot()

# outra coisa que funciona, e ajuda a cortar caminho
cetesb_pinheiros_ts |>
  gg_season()

# até seria possível fazer de outro jeito, mas o CONTEXTO
# permite que os pacotes já se preparem para
# nos dar o que a gente precisa

