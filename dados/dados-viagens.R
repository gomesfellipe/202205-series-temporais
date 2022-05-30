json <- jsonlite::read_json("https://sistemas.anac.gov.br/dadosabertos/Voos%20e%20opera%C3%A7%C3%B5es%20a%C3%A9reas/Dados%20Estat%C3%ADsticos%20do%20Transporte%20A%C3%A9reo/Dados_Estatisticos_2011_a_2020.json")

library(tidyverse)

x <- readr::read_csv2(
  "https://sistemas.anac.gov.br/dadosabertos/Voos%20e%20opera%C3%A7%C3%B5es%20a%C3%A9reas/Dados%20Estat%C3%ADsticos%20do%20Transporte%20A%C3%A9reo/Dados_Estatisticos.csv",
  skip = 1)

dados <- x %>%
  filter(GRUPO_DE_VOO == "REGULAR", AEROPORTO_DE_ORIGEM_SIGLA %in% c("SBGR", "SBSP")) %>%
  group_by(ANO, MES) %>%
  summarise(.groups = "drop",
    PASSAGEIROS_PAGOS = sum(PASSAGEIROS_PAGOS, na.rm = TRUE),
    CARGA_PAGA_KG = sum(CARGA_PAGA_KG, na.rm = TRUE)
  ) %>%
  mutate(
    DATA = lubridate::make_date(ANO, MES)
  )

dados_regiao <- x %>%
  filter(
    (!is.na(AEROPORTO_DE_ORIGEM_UF) | !is.na(AEROPORTO_DE_DESTINO_UF))
  ) %>%
  mutate(
    AEROPORTO_DE_DESTINO_REGIAO = dplyr::if_else(
      is.na(AEROPORTO_DE_DESTINO_REGIAO),
      "EXTERIOR",
      AEROPORTO_DE_DESTINO_REGIAO),
    AEROPORTO_DE_DESTINO_UF = dplyr::if_else(
      is.na(AEROPORTO_DE_DESTINO_UF),
      "EXTERIOR",
      AEROPORTO_DE_DESTINO_UF)
  ) |>
  group_by(ANO, MES, AEROPORTO_DE_DESTINO_REGIAO, AEROPORTO_DE_DESTINO_UF) %>%
  summarise(.groups = "drop",
            PASSAGEIROS_PAGOS = sum(PASSAGEIROS_PAGOS, na.rm = TRUE),
            CARGA_PAGA_KG = sum(CARGA_PAGA_KG, na.rm = TRUE)
  ) %>%
  mutate(
    DATA = lubridate::make_date(ANO, MES)
  )

saveRDS(dados, "dados/anac-sp.rds")
saveRDS(dados_regiao, "dados/anac-br.rds")


