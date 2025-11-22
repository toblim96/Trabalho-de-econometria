# ================================================================
# ARQUIVO: 4_graficos_analise.R
# TRABALHO: Econometria 1
# PROFESSOR: Vitor Pereira
# OBJETIVO: Criar visualizações gráficas dos resultados
# INPUT: dados_simulados.csv (pasta tmp/)
# OUTPUT:
#   - grafico_salarios.png (pasta output/)
#   - grafico_educacao.png (pasta output/)
#   - grafico_experiencia.png (pasta output/)
# DATA: 2024
# ================================================================

# --- CARREGAR CONFIGURAÇÕES ---
source("code/_master.R")

# --- CARREGAR PACOTES ---
library(ggplot2)
library(dplyr)

cat("✅ 4_graficos_analise.R: Pronto para criação de gráficos\\n")
