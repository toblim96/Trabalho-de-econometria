# ==============================================================================
# Analise de Repasse Cambial - Questao 2
# Trabalho de Econometria I - IE/UFRJ
# Grupo: Juliana, crisly e tobias
# Data: Novembro de 2025
#
# Objetivo: Estimar o repasse cambial para inflacao do setor 13
# 
# INPUT: T2dados.xls (pasta input)
# OUTPUT: Tabelas e graficos (pasta output)
# ==============================================================================
rm(list = ls())
source("code/_master.R")

# Pacotes
library(readxl)
library(dplyr)
library(lmtest)
library(sandwich)
library(stargazer)
library(ggplot2)

# Carregar dados
dados <- read_excel(file.path(path_input, "T2dados.xls"))
dados <- dados %>% rename(setor = setor13)

# Item 2.a - Modelo básico
modelo1 <- lm(setor ~ cambio + ibcbr, data = dados)
summary(modelo1)

stargazer(modelo1, type = "html",
          out = file.path(path_output, "2a_modelo_basico.html"),
          title = "Modelo Básico - Setor 13",
          digits = 4)

# Item 2.b - Interpretação
beta_cambio <- coef(modelo1)["cambio"]
beta_ibcbr <- coef(modelo1)["ibcbr"]

cat("\nRepasse cambial:", round(beta_cambio, 4), "\n")

# Item 2.c - Teste Ljung-Box
ljung1 <- Box.test(residuals(modelo1), lag = 12, type = "Ljung-Box")
cat("\nLjung-Box p-valor:", round(ljung1$p.value, 4))
cat("\nAutocorrelação?", ifelse(ljung1$p.value < 0.05, "Sim", "Não"), "\n")

# Item 2.d - Dummies sazonais
dados$mes <- rep(1:12, length.out = nrow(dados))
for(i in 2:12) {
  dados[[paste0("mes", i)]] <- ifelse(dados$mes == i, 1, 0)
}

modelo2 <- lm(setor ~ cambio + ibcbr + mes2 + mes3 + mes4 + mes5 + mes6 + 
                mes7 + mes8 + mes9 + mes10 + mes11 + mes12, data = dados)

stargazer(modelo1, modelo2, type = "html",
          out = file.path(path_output, "2d_sazonal.html"),
          column.labels = c("Básico", "Com Sazonalidade"),
          digits = 4)

# Item 2.e - Comparação
cat("\nMudança no coef. câmbio:", 
    round(coef(modelo2)["cambio"] - coef(modelo1)["cambio"], 4), "\n")

# Item 2.f - Ljung-Box modelo 2
ljung2 <- Box.test(residuals(modelo2), lag = 12, type = "Ljung-Box")
cat("P-valor modelo 2:", round(ljung2$p.value, 4), "\n")

# Item 2.g - Defasagens
dados <- dados %>%
  mutate(setor_lag1 = lag(setor, 1),
         cambio_lag1 = lag(cambio, 1))

modelo3 <- lm(setor ~ cambio + ibcbr + setor_lag1 + cambio_lag1 + 
                mes2 + mes3 + mes4 + mes5 + mes6 + mes7 + mes8 + 
                mes9 + mes10 + mes11 + mes12, data = dados)

# Impacto total
imp_atual <- coef(modelo3)["cambio"]
imp_defas <- coef(modelo3)["cambio_lag1"]
imp_total <- imp_atual + imp_defas

cat("\nImpacto contemporâneo:", round(imp_atual, 4))
cat("\nImpacto defasado:", round(imp_defas, 4))
cat("\nImpacto total:", round(imp_total, 4), "\n")

stargazer(modelo1, modelo2, modelo3, type = "html",
          out = file.path(path_output, "2g_completo.html"),
          column.labels = c("Básico", "Sazonal", "Defasagens"),
          digits = 4)

# Item 2.h - Testes
ljung3 <- Box.test(residuals(modelo3), lag = 12, type = "Ljung-Box")
bp <- bptest(modelo3)

cat("\nLjung-Box p-valor:", round(ljung3$p.value, 4))
cat("\nBreusch-Pagan p-valor:", round(bp$p.value, 4), "\n")

# Item 2.i - Erros HAC
m3_hac <- coeftest(modelo3, vcov = vcovHAC(modelo3))

pv_padrao <- summary(modelo3)$coef["cambio", "Pr(>|t|)"]
pv_hac <- m3_hac["cambio", "Pr(>|t|)"]

cat("\nP-valor padrão:", round(pv_padrao, 4))
cat("\nP-valor HAC:", round(pv_hac, 4), "\n")

stargazer(modelo3, modelo3, type = "html",
          se = list(NULL, sqrt(diag(vcovHAC(modelo3)))),
          out = file.path(path_output, "2i_hac.html"),
          column.labels = c("Erros Padrão", "Erros HAC"),
          digits = 4)

cat("\nAnálise concluída!\n")