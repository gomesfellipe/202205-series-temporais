library(fable)
library(feasts)
library(timetk)
library(modeltime)
library(tidyverse)

cerveja <- readRDS("dados/cerveja.rds") |>
  mutate(DATA = tsibble::yearmonth(paste(CO_ANO, CO_MES)),
         DATA_dt = lubridate::make_date(CO_ANO, CO_MES))

cerveja_ts <- cerveja %>%
  as_tsibble(index = DATA) |>
  select(KG_LIQUIDO, everything())

autoplot(cerveja_ts, KG_LIQUIDO)

autoplot(stl_components)

gg_season(cerveja_ts)

stl_components <- cerveja_ts %>%
  model(
    STL(KG_LIQUIDO ~ season(12) + trend())
  ) %>%
  components()

stl_components |>
  autoplot()

stl_components %>%
  ACF(remainder) %>%
  autoplot()

# Modelo

split <- time_series_split(
  cerveja,
  DATA_dt,
  initial = "23 year",
  assess = "2 year"
)

plot_time_series_cv_plan(
  tk_time_series_cv_plan(split),
  DATA_dt, KG_LIQUIDO)

# Definição do modelo inicial

model_spec <- parsnip::linear_reg() %>%
  set_engine("lm")

# AJuste do modelo - por enquanto não vamos usá-lo
fitted <- model_spec |>
  fit(KG_LIQUIDO ~ CO_ANO + CO_MES, training(split))

fitted_lag <- model_spec |>
  fit(KG_LIQUIDO ~ CO_ANO + CO_MES + I(lag(KG_LIQUIDO, 1, 0)), training(split))

fitted_lag_sem_season <- model_spec |>
  fit(KG_LIQUIDO ~ CO_ANO + I(lag(KG_LIQUIDO, 1, 0)), training(split))

fitted_lag_1_e_2 <- model_spec |>
  fit(KG_LIQUIDO ~ CO_ANO +
        I(lag(KG_LIQUIDO, 1, 0)) +
        I(lag(KG_LIQUIDO, 2, 0)), training(split))

fitted_lag_1_a_3 <- model_spec |>
  fit(KG_LIQUIDO ~ CO_ANO +
        I(lag(KG_LIQUIDO, 1, 0))+
        I(lag(KG_LIQUIDO, 2, 0))+
        I(lag(KG_LIQUIDO, 3, 0)), training(split))


models_tbl <- modeltime_table(
  fitted,
  fitted_lag,
  fitted_lag_sem_season,
  fitted_lag_1_e_2,
  fitted_lag_1_a_3
)

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(split))

forecasts <- calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(split),
    actual_data = cerveja
  )

calibration_tbl %>%
  modeltime_accuracy()

plot_modeltime_forecast(forecasts)

modeltime_residuals(calibration_tbl) |>
  plot_modeltime_residuals()

modeltime_residuals(calibration_tbl) |>
  group_by(.model_id) |>
  plot_acf_diagnostics(.index, .residuals)

model_arima <- modeltime::arima_reg(
  #non_seasonal_ar = 1
  ) |>
  set_engine("auto_arima")

fit_arima <- model_arima |>
  fit(KG_LIQUIDO ~ DATA_dt, data = training(split))

treino <- training(split)$KG_LIQUIDO


plot(treino)
arima_fit_forecast <- forecast::Arima(treino, order = c(1,0,0))

plot(forecast::forecast(arima_fit_forecast))

models_tbl <- modeltime_table(
  fitted,
  #fitted_lag_1_a_3,
  fit_arima
)

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(split))

forecasts <- calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(split),
    actual_data = cerveja
  )

plot_modeltime_forecast(forecasts)

calibration_tbl |>
  modeltime_residuals() |>
  plot_modeltime_residuals()

modeltime_residuals(calibration_tbl) |>
  group_by(.model_id) |>
  plot_acf_diagnostics(.index, .residuals)

calibration_tbl %>%
  modeltime_accuracy()

# outra possibilidade

# devtools::install_github("beatrizmilz/mananciais")

chuva_tiete <- mananciais::mananciais |>
  filter(sistema == "Alto Tietê") |>
  group_by(mes = lubridate::floor_date(data, "month")) |>
  summarise(pluviometria_dia = sum(pluviometria_dia))

