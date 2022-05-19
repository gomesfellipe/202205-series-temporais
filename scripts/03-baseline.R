
# Carregar dados ----------------------------------------------------------

cetesb_pinheiros_co <- readRDS("dados/cetesb_pinheiros_diario_co.rds") |>
  mutate(
    mes = lubridate::month(data),
    Mes = lubridate::floor_date(data, "month")
  )

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





