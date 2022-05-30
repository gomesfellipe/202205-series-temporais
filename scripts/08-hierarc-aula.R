library(fable)
library(feasts)
library(timetk)
library(modeltime)
library(tidymodels)
library(tsibble)
library(tidyverse)

anac_regiao <- readr::read_rds("https://github.com/curso-r/main-series/blob/main/dados/anac-br.rds?raw=true") %>%
  filter(
    AEROPORTO_DE_DESTINO_REGIAO != "NÃO IDENTIFICADO") |>
  rename(
    REGIAO = AEROPORTO_DE_DESTINO_REGIAO,
    UF = AEROPORTO_DE_DESTINO_UF
  ) |>
  filter(!(REGIAO == "NORDESTE" & UF == "MG")) |>
  mutate(DATA_ym = tsibble::yearmonth(paste(ANO, MES, sep = "-"))) %>%
  mutate(
    TEMPO_DESDE_INICIO = difftime(
      DATA,
      lubridate::ymd("1999-12-01"),
      units = "days"
    )/30,
    LAG_1 = lag(PASSAGEIROS_PAGOS, 1, default = 0),
    CARGA_LAG = lag(CARGA_PAGA_KG),
    dif_CARGA_LAG = CARGA_LAG - lag(CARGA_LAG, 2)
  ) |>
  filter(DATA <= as.Date("2018-12-31"))

anac_regiao_ts <- anac_regiao |>
  as_tsibble(
    index = DATA_ym,
    key = c(REGIAO, UF)
  )

anac_regiao_ts |>
  autoplot(
    PASSAGEIROS_PAGOS
  )

anac_regiao_ts |>
  filter(REGIAO == "NORDESTE",
         UF %in% c("AL", "BA", "CE")) |>
  gg_season(
    PASSAGEIROS_PAGOS
  )

anac_regiao_ts |>
  filter(REGIAO == "EXTERIOR") |>
  gg_tsdisplay(
    lag_max = 60,
    PASSAGEIROS_PAGOS, plot_type = "partial"
  )

anac_regiao_nested <- anac_regiao |>
  mutate(
    id_serie = paste0(REGIAO, "/", UF)
  ) |>
  extend_timeseries(
    .id_var = id_serie,
    .date_var = DATA,
    .length_future = 12
  )  |>
  nest_timeseries(
    .id_var = id_serie,
    .length_future = 12,
    .length_actual = 228
  ) |>
  split_nested_timeseries(
    .length_test = 24
  )

# pre-processamento

pre_proc_arima <- recipe(
  PASSAGEIROS_PAGOS ~ DATA,
  extract_nested_train_split(anac_regiao_nested)
)

# especificacao do modelo

arima_spec <- modeltime::arima_reg() |>
  set_engine("auto_arima")

ets_spec <- modeltime::seasonal_reg() |>
  set_engine("tbats")

# juntando tudo: criando o workflow

wflw_arima <- workflow() |>
  add_model(arima_spec) |>
  add_recipe(pre_proc_arima)

wflw_ets <- workflow() |>
  add_model(ets_spec) |>
  add_recipe(pre_proc_arima)

modelos_ajustados <- modeltime_nested_fit(
  nested_data = anac_regiao_nested,
  wflw_arima,
  wflw_ets
)

modelos_ajustados |>
  extract_nested_test_forecast() |>
  filter(id_serie == "CENTRO-OESTE/DF") |>
  plot_modeltime_forecast()

modelos_ajustados |>
  modeltime_nested_select_best(metric = "mape") |>
  extract_nested_test_forecast()

modelos_ajustados |>
  modeltime_nested_select_best(metric = "mape") |>
  extract_nested_test_forecast() |>
  group_by(
    .index,
    .key
  ) |>
  summarise(
    .value = sum(.value)
  ) |>
  ungroup() |>
  pivot_wider(
    names_from = .key,
    values_from = .value
  ) |>
  summarise(
    MASE = mase_vec(actual, prediction),
    MAPE = mape_vec(actual, prediction),
    RSQ = rsq_vec(actual, prediction)
  )


