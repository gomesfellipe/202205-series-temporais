library(fable)
library(feasts)
library(tidyverse)

# Dados de exportação brasileira de soja obtidos do ComexBrasil.

soja <- readr::read_rds("https://github.com/curso-r/main-series/raw/main/dados/soja.rds")

soja_ts <- soja %>%
  mutate(DATA = tsibble::yearmonth(paste(CO_ANO, CO_MES))) %>%
  as_tsibble(index = DATA)

autoplot(soja_ts, KG_LIQUIDO)

# Médias móveis -----------------------------------------------------------

x <- 1:100
x %>% slider::slide_dbl(
  .before = 6,
  .after = 6,
  mean,
  complete = TRUE
)
mean(x[1:7])

soja_ts %>%
  mutate(
    # aqui escolhemos 6 antes, mas o ideal seria pegar a média considerando 12
    # meses
    MM = slider::slide_dbl(KG_LIQUIDO, .before = 6, .after = 6, mean, complete = TRUE)
  ) %>%
  autoplot(KG_LIQUIDO) +
  #ggplot(aes(x = DATA)) +
  geom_line(aes(y = MM), color = "red")

soja_ts %>%
  mutate(
    # aqui escolhemos 5 antes e 6 depois e depois tiramos a média novamente
    # meses
    MM1 = KG_LIQUIDO %>%
      slider::slide_dbl(.before = 5, .after = 6, mean, .complete = TRUE),
    MM = MM1 %>%
      slider::slide_dbl(.before = 1, .after = 0, mean, complete = TRUE)
  ) %>%
  autoplot(KG_LIQUIDO) +
  geom_line(aes(y = MM), color = "red")


# Decomposição clássica ---------------------------------------------------

decomposicao <- soja_ts %>%
  mutate(
    # aqui escolhemos 5 antes e 6 depois e depois tiramos a média novamente
    # meses
    tendencia = KG_LIQUIDO %>%
      slider::slide_dbl(.before = 5, .after = 6, mean, complete = TRUE) %>%
      slider::slide_dbl(.before = 1, .after = 0, mean, complete = TRUE),
    estacionaria = KG_LIQUIDO - tendencia
  ) %>%
  #tibble::as_tibble() %>%
  group_by(CO_MES) %>%
  mutate(
    sazonalidade = mean(estacionaria, na.rm = TRUE),
    residuo = estacionaria - sazonalidade
  ) %>%
  ungroup()

decomposicao %>%
  select(DATA, KG_LIQUIDO, tendencia, sazonalidade, residuo) %>%
  pivot_longer(
    cols = c(KG_LIQUIDO, tendencia, sazonalidade, residuo),
    names_to = "serie",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = as.Date(DATA), y = valor)) +
  geom_line() +
  facet_wrap(ncol = 1, ~serie, scales = "free")

# o feasts tem uma função para fazer isso automaticamente.

soja_ts %>%
  model(
    classical_decomposition(KG_LIQUIDO, type = "multiplicative")
  ) %>%
  components() %>%
  autoplot()

# O que seria bom?

# Pouca autocorelação nos resíduos. Se ainda existe autocorrelação quer
# dizer que ainda existem componentes temporais que o nosso modelo não
# consegue capturar.
soja_ts %>%
  model(
    classical_decomposition(KG_LIQUIDO, type = "additive")
  ) %>%
  components() %>%
  ACF(random) %>%
  autoplot()

# STL ajuda?
stl_components <- soja_ts %>%
  model(
    STL(KG_LIQUIDO ~ season(12) + trend())
  ) %>%
  components()

autoplot(stl_components)

stl_components %>%
  ACF(remainder) %>%
  autoplot()




