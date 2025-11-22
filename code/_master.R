# ================================================================
# ARQUIVO: _master.R
# TRABALHO: Trabalho Econometria I - IE/UFRJ
# Juliana, Crisly e Tobias
# Novembro 2025
# ================================================================

# --- CONFIGURAÇÃO DE CAMINHOS --- 
# ⚠️ AJUSTE ESTE CAMINHO SE MUDAR DE COMPUTADOR ⚠️
caminho_raiz <- "C:/Users/tobia/Desktop/Trabalho de econometria"

# --- DEFINIR TODOS OS DIRETÓRIOS ---
code_dir   <- file.path(caminho_raiz, "code")
input_dir  <- file.path(caminho_raiz, "input")
tmp_dir    <- file.path(caminho_raiz, "tmp")
output_dir <- file.path(caminho_raiz, "output")
misc_dir   <- file.path(caminho_raiz, "misc")

# --- CARREGAR PACOTES ESSENCIAIS ---
# Instalar pacotes se necessário
if (!require(stargazer)) install.packages("stargazer")
if (!require(dplyr)) install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(lmtest)) install.packages("lmtest")
if (!require(sandwich)) install.packages("sandwich")

# Carregar pacotes
library(stargazer)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)

# --- VERIFICAR/CRIAR PASTAS ---
dir.create(input_dir, showWarnings = FALSE)
dir.create(tmp_dir, showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)
dir.create(misc_dir, showWarnings = FALSE)

# --- MENSAGEM DE CONFIRMAÇÃO ---
cat("✅ Estrutura de pastas carregada!\n")
cat("📁 Pasta principal:", caminho_raiz, "\n")
cat("📦 Pacotes essenciais carregados\n")

