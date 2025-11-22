# ================================================================
# ARQUIVO: 2_estatisticas_descritivas.R
# TRABALHO: Econometria 1  
# PROFESSOR: Vitor Pereira
# OBJETIVO: Gerar tabelas de estatísticas descritivas da base
# INPUT: dados_simulados.csv (pasta tmp/)
# OUTPUT: 
#   - tabela_descritivas.html (pasta output/)
#   - sumario_estatistico.txt (pasta output/)
# DATA: 2024
# ================================================================

# --- CARREGAR CONFIGURAÇÕES ---
source("code/_master.R")

# --- CARREGAR PACOTES ---
library(stargazer)
library(dplyr)

cat("✅ 2_estatisticas_descritivas.R: Pronto para análise descritiva\\n")
