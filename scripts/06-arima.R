library(fable)
library(feasts)
library(timetk)
library(modeltime)
library(tidymodels)
library(tidyverse)

anac <- readr::read_rds("https://github.com/curso-r/main-series/blob/main/dados/anac-sp.rds?raw=true") %>%
  mutate(DATA_ym = tsibble::yearmonth(paste(ANO, MES, sep = "-"))) %>%
  mutate(
    TEMPO_DESDE_INICIO = difftime(
      DATA,
      lubridate::ymd("1999-12-01"),
      units = "days"
    )/30,
    LAG_1 = lag(PASSAGEIROS_PAGOS, 1, default = 0),
    CARGA_LAG = lag(CARGA_PAGA_KG),
  ) |>
  filter(DATA <= as.Date("2018-12-31"))

split <- time_series_split(
  anac,
  DATA,
  initial = "18 years",
  assess = "1 years"
)

plot_time_series_cv_plan(
  tk_time_series_cv_plan(split),
  DATA, PASSAGEIROS_PAGOS)

regressao_spec <- parsnip::linear_reg() %>%
  set_engine("lm")

stl_spec <- modeltime::seasonal_reg()

arima_spec <- modeltime::arima_reg() |>
  set_engine("auto_arima")

regressao <- regressao_spec |>
  fit(PASSAGEIROS_PAGOS ~ TEMPO_DESDE_INICIO + as.factor(MES), training(split))

STL <- stl_spec |>
  fit(PASSAGEIROS_PAGOS ~ DATA, training(split))

arima <- arima_spec |>
  fit(PASSAGEIROS_PAGOS ~ DATA, training(split))

models_tbl <- modeltime_table(
  regressao,
  STL,
  arima
)

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(split))

calibration_tbl

forecasts <- calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(split),
    actual_data = anac
  )

calibration_tbl %>%
  modeltime_accuracy()

plot_modeltime_forecast(forecasts)
