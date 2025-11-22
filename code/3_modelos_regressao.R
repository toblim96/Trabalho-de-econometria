# ================================================================
# ARQUIVO: 3_modelos_regressao.R
# TRABALHO: Econometria 1
# PROFESSOR: Vitor Pereira  
# OBJETIVO: Estimar modelos de regressão para retornos à educação
# INPUT: dados_simulados.csv (pasta tmp/)
# OUTPUT:
#   - resultados_regressao.html (pasta output/)
#   - tabelas_latex.tex (pasta output/)
# DATA: 2024
# ================================================================

# --- CARREGAR CONFIGURAÇÕES ---
source("code/_master.R")

# --- CARREGAR PACOTES ---
library(lmtest)
library(sandwich)
library(stargazer)

cat("✅ 3_modelos_regressao.R: Pronto para estimação de modelos\\n")
