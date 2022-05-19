
# Carregar dados ----------------------------------------------------------

cetesb_pinheiros_co <- readr::read_rds("dados/cetesb_pinheiros_diario_co.rds")

# Intro timetk/modeltime --------------------------------------------------

library(timetk)
library(modeltime)

cetesb_pinheiros_co %>%
  plot_time_series(data, concentracao)

cetesb_pinheiros_co %>%
  plot_time_series_boxplot(data, concentracao, .period = "1 month")

cetesb_pinheiros_co %>%
  plot_seasonal_diagnostics(data, concentracao)

cetesb_pinheiros_co %>%
  plot_acf_diagnostics(data, concentracao)

# não funciona pois temos missing
cetesb_pinheiros_co %>%
  plot_stl_diagnostics(data, concentracao)

# Tidy --------------------------------------------------------------------

# missing implícito...

cetesb_pinheiros_co_implicito <- cetesb_pinheiros_co %>%
  filter(!is.na(concentracao))

# transforma missing implícitos em missing explícitos.
cetesb_pinheiros_co_implicito <- cetesb_pinheiros_co_implicito %>%
  timetk::pad_by_time(data, .by = "day")

sum(is.na(cetesb_pinheiros_co_implicito$concentracao))

cetesb_pinheiros_co_implicito %>%
  plot_time_series(data, concentracao)

# Essa base possui observações com missing
summary(cetesb_pinheiros_co$concentracao)

imputacoes <- cetesb_pinheiros_co %>%
  mutate(
    concentracao_linear = ts_impute_vec(concentracao, period = 1, lambda = NULL),
    concentracao_boxcox = ts_impute_vec(concentracao, period = 1, lambda = 2),
    concentracao_sazonal = ts_impute_vec(concentracao, period = 365)
  )

imputacoes %>%
  select(data, starts_with("concentracao")) %>%
  pivot_longer(
    cols = starts_with("concentracao"),
    names_to = "imputacao",
    values_to = "value"
  ) %>%
  plot_time_series(data, value, .facet_var = imputacao)

# inputacao final

cetesb_pinheiros_co <- cetesb_pinheiros_co %>%
  mutate(
    concentracao = ts_impute_vec(concentracao, period = 365)
  )


# Backtesting -------------------------------------------------------------

split <- time_series_split(
  cetesb_pinheiros_co,
  data,
  initial = "40 months",
  assess = "1 month"
)

backtest <- time_series_cv(
  training(split),
  data,
  initial = "36 months",
  assess = "1 month",
  slice_limit = 2
)

split
backtest
plot_time_series_cv_plan(backtest, data, concentracao)

# Ajustando o modelo ------------------------------------------------------


library(tidymodels)
library(modeltime.resample)

# Definição do modelo inicial

model_spec <- modeltime::naive_reg() %>%
  set_engine("naive")

# AJuste do modelo - por enquanto não vamos usá-lo
fitted <- model_spec %>%
  fit(concentracao ~ data, training(split))

# Colocar o modelo na model_tbl
models_tbl <- modeltime_table(
  fitted
)

# Re ajustar o modelo para as amostras do backtesting
resamples_fitted <- models_tbl  %>%
  modeltime_fit_resamples(
    resamples = backtest,
    control   = control_resamples(verbose = FALSE)
  )

# Visualizar os resultados
resamples_fitted %>%
  plot_modeltime_resamples(.metric_set = metric_set(mae, mape, mase))

resamples_fitted %>%
  modeltime_resample_accuracy(
    summary_fns = mean,
    metric_set = metric_set(mae, mape, mase)
  )

#....
# uma vez contentes com os modelos podemos gerar previsões p/ a base de teste
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(split))

forecasts <- calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(split),
    actual_data = cetesb_pinheiros_co
  )

calibration_tbl %>%
  modeltime_accuracy()

# Agora vamos refitar o modelo com a base toda

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = cetesb_pinheiros_co)

forecast_futuro <- refit_tbl %>%
  modeltime_forecast(h = "1 month", actual_data = cetesb_pinheiros_co)

forecast_futuro %>%
  plot_modeltime_forecast()
