# Faça a decomposição da série de milho e analise os resultados.
# Esse banco de dados contém dados de exportação de milho do Brasil em KG.

milho <- readr::read_rds("https://github.com/curso-r/main-series/raw/main/dados/milho.rds")

# Como é a sazonalidade dessa série?
# Qual é a tendência?
# Como se comportam os resíduos da decomposição?
