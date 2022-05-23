# Baixar dados do comex aqui:
# https://www.gov.br/produtividade-e-comercio-exterior/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta

df <- readr::read_csv2("D:/EXP_COMPLETA.csv")
ncm <- readr::read_csv2("D:/NCM.csv")
ncm_sh <- readr::read_csv2("D:/NCM_SH.csv", locale = locale(encoding = "latin1"))

extrai_serie_co_sh4 <- function(dados, valor){
  df %>%
    left_join(ncm, by = c("CO_NCM", "CO_UNID")) %>%
    left_join(ncm_sh, by = c("CO_SH6")) %>%
    filter(CO_SH4 == valor) %>%
    group_by(CO_SH4, NO_SH4_POR, CO_ANO, CO_MES) %>%
    summarise(
      across(c(QT_ESTAT, KG_LIQUIDO, VL_FOB), .fns = sum, .names = "{.col}"),
      .groups = "drop"
    )
}

cerveja_sem_alcool <-  extrai_serie_co_sh4(df, "2202")

soja <- df %>%
  left_join(ncm, by = c("CO_NCM", "CO_UNID")) %>%
  left_join(ncm_sh, by = c("CO_SH6")) %>%
  filter(CO_SH4 == "1201") %>%
  group_by(CO_SH4, NO_SH4_POR, CO_ANO, CO_MES) %>%
  summarise(
    across(c(QT_ESTAT, KG_LIQUIDO, VL_FOB), .fns = sum, .names = "{.col}"),
    .groups = "drop"
  )

milho <- df %>%
  left_join(ncm, by = c("CO_NCM", "CO_UNID")) %>%
  left_join(ncm_sh, by = c("CO_SH6")) %>%
  filter(CO_SH4 == "1005") %>%
  group_by(CO_SH4, NO_SH4_POR, CO_ANO, CO_MES) %>%
  summarise(
    across(c(QT_ESTAT, KG_LIQUIDO, VL_FOB), .fns = sum, .names = "{.col}"),
    .groups = "drop"
  )

cerveja <- df |>
  filter(CO_NCM == "22030000", CO_UNID == "17") |>
  group_by(CO_ANO, CO_MES) %>%
  summarise(
    across(c(QT_ESTAT, KG_LIQUIDO, VL_FOB), .fns = sum, .names = "{.col}"),
    .groups = "drop"
  ) |>
  ungroup()


saveRDS(soja, "dados/soja.rds")
saveRDS(milho, "dados/milho.rds")
saveRDS(cerveja, "dados/cerveja.rds")

milho %>%
  mutate(date = lubridate::make_date(CO_ANO, CO_MES)) %>%
  ggplot(aes(x = date, y = KG_LIQUIDO/1000)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_number(
    scale_cut = scales::cut_short_scale()
  ))

cerveja_sem_alcool |>
  mutate(date = lubridate::make_date(CO_ANO, CO_MES)) %>%
  ggplot(aes(x = date, y = KG_LIQUIDO/1000)) +
  geom_line() #+
  #scale_y_continuous(labels = scales::label_number(
  #  scale_cut = scales::cut_short_scale()
  #))



View(ncm_sh)
head(df)
