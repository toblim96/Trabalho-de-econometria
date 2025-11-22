# ================================================================
# ARQUIVO: 20_questao_1.11.R
# TRABALHO: Econometria 1
# PROFESSOR: Vitor Pereira
# GRUPO: 1
# OBJETIVO: Reestimar equação minceriana por FGLS e comparar com parâmetros verdadeiros
# INPUT: dados_simulados.csv (pasta tmp/)
# OUTPUT:
#   - comparacao_fgls_1.11.html (pasta output/)
#   - relatorio_fgls_1.11.txt (pasta output/)
# DATA: Novembro de 2024
# ================================================================

# --- CARREGAR CONFIGURAÇÕES ---
source("code/_master.R")

cat("QUESTÃO 1.11: ESTIMAÇÃO POR FGLS E COMPARAÇÃO COM PARÂMETROS VERDADEIROS\n")
cat("======================================================================\n\n")

# --- EXPLICAÇÃO DA QUESTÃO ---
cat("OBJETIVO: Reestimar a equação de Mincer por FGLS e verificar se as estimativas\n")
cat("          ficaram mais próximas dos parâmetros verdadeiros usados na simulação\n\n")

cat("MÉTODO FGLS (Feasible Generalized Least Squares):\n")
cat("1. Estimar modelo OLS inicial\n")
cat("2. Calcular variância dos resíduos em função dos fitted values\n") 
cat("3. Reestimar modelo com pesos = 1/variância estimada\n")
cat("4. Comparar OLS vs FGLS vs Parâmetros Verdadeiros\n\n")

# --- CARREGAR DADOS ---
cat("1. CARREGANDO DADOS...\n")
dados <- read.csv(file.path(tmp_dir, "dados_simulados.csv"))

# Preparar dados
dados_preparados <- dados %>%
  mutate(experience_sq = experience^2) %>%
  select(log_wage_hetero, education, experience, experience_sq,
         north, northeast, south, centerwest, male, white)

cat("   ✅ Dados carregados:", nrow(dados_preparados), "observações\n")

# --- PARÂMETROS VERDADEIROS (USADOS NA GERAÇÃO DOS DADOS) ---
cat("\n2. DEFININDO PARÂMETROS VERDADEIROS...\n")
parametros_verdadeiros <- c(
  "(Intercept)" = 6.8,
  "education" = 0.13,
  "experience" = 0.035, 
  "experience_sq" = -0.0005,
  "north" = -0.15,
  "northeast" = -0.20,
  "south" = 0.08,
  "centerwest" = 0.05,
  "male" = 0.25,
  "white" = 0.18
)

cat("   ✅ Parâmetros verdadeiros definidos (usados na simulação)\n")

# --- ESTIMAR MODELO OLS ---
cat("\n3. ESTIMANDO MODELO OLS...\n")
modelo_ols <- lm(log_wage_hetero ~ education + experience + experience_sq +
                   north + northeast + south + centerwest + male + white,
                 data = dados_preparados)

cat("   ✅ Modelo OLS estimado:\n")
cat("   - R²:", round(summary(modelo_ols)$r.squared, 4), "\n")
cat("   - Observações:", nrow(dados_preparados), "\n")

# --- IMPLEMENTAR FGLS ---
cat("\n4. IMPLEMENTANDO FGLS...\n")

implementar_fgls <- function(modelo_ols, dados) {
  # Passo 1: Obter resíduos do OLS
  residuos <- residuals(modelo_ols)
  
  # Passo 2: Estimar variância dos resíduos
  cat("   a) Estimando variância dos resíduos...\n")
  modelo_variancia <- lm(I(residuos^2) ~ fitted(modelo_ols) + I(fitted(modelo_ols)^2))
  sigma2_hat <- fitted(modelo_variancia)
  
  # Passo 3: Evitar divisão por zero
  sigma2_hat <- pmax(sigma2_hat, 1e-6)
  
  # Passo 4: Estimar FGLS com pesos
  cat("   b) Estimando FGLS com pesos...\n")
  modelo_fgls <- lm(log_wage_hetero ~ education + experience + experience_sq +
                      north + northeast + south + centerwest + male + white,
                    data = dados, weights = 1/sigma2_hat)
  
  return(modelo_fgls)
}

modelo_fgls <- implementar_fgls(modelo_ols, dados_preparados)
cat("   ✅ Modelo FGLS estimado com sucesso\n")

# --- COMPARAR RESULTADOS ---
cat("\n5. COMPARANDO OLS vs FGLS vs VERDADEIROS...\n")

# Criar tabela comparativa
comparacao <- data.frame(
  Variavel = names(coef(modelo_ols)),
  Verdadeiro = round(parametros_verdadeiros[names(coef(modelo_ols))], 4),
  OLS = round(coef(modelo_ols), 4),
  FGLS = round(coef(modelo_fgls), 4),
  Erro_OLS = round(abs(coef(modelo_ols) - parametros_verdadeiros[names(coef(modelo_ols))]), 4),
  Erro_FGLS = round(abs(coef(modelo_fgls) - parametros_verdadeiros[names(coef(modelo_ols))]), 4)
)

# Calcular estatísticas de erro
erro_medio_ols <- mean(comparacao$Erro_OLS)
erro_medio_fgls <- mean(comparacao$Erro_FGLS)
melhoria <- ((erro_medio_ols - erro_medio_fgls) / erro_medio_ols) * 100

# --- SALVAR RESULTADOS EM HTML ---
cat("\n6. REPORTANDO RESULTADOS COM STARGAZER...\n")

stargazer(
  comparacao,
  type = "html",
  title = "QUESTÃO 1.11: Comparação OLS vs FGLS - Proximidade dos Parâmetros Verdadeiros",
  summary = FALSE,
  rownames = FALSE,
  notes = paste("Erro médio OLS:", round(erro_medio_ols, 4), 
                "| Erro médio FGLS:", round(erro_medio_fgls, 4),
                "| Melhoria:", round(melhoria, 1), "%"),
  out = file.path(output_dir, "comparacao_fgls_1.11.html")
)

# --- SALVAR RELATÓRIO DETALHADO ---
cat("\n7. GERANDO RELATÓRIO DETALHADO...\n")
sink(file.path(output_dir, "relatorio_detalhado_1.11.txt"))

cat("RELATÓRIO DETALHADO - QUESTÃO 1.11\n")
cat("==================================\n\n")

cat("ANÁLISE FGLS vs OLS - PROXIMIDADE DOS PARÂMETROS VERDADEIROS\n")
cat("-----------------------------------------------------------\n\n")

cat("METODOLOGIA FGLS:\n")
cat("1. Estimação OLS inicial\n")
cat("2. Modelagem da variância: resíduos² ~ fitted + fitted²\n")
cat("3. Pesos = 1/variância_estimada\n")
cat("4. Reestimação com Mínimos Quadrados Ponderados\n\n")

cat("RESULTADOS DA COMPARAÇÃO:\n")
cat("-------------------------\n")
cat("Erro médio absoluto - OLS:", round(erro_medio_ols, 4), "\n")
cat("Erro médio absoluto - FGLS:", round(erro_medio_fgls, 4), "\n")
cat("Melhoria do FGLS sobre OLS:", round(melhoria, 1), "%\n\n")

cat("ANÁLISE POR VARIÁVEL:\n")
cat("---------------------\n")
for(i in 1:nrow(comparacao)) {
  cat(comparacao$Variavel[i], ":\n")
  cat("  Verdadeiro:", comparacao$Verdadeiro[i], "\n")
  cat("  OLS:", comparacao$OLS[i], " (Erro:", comparacao$Erro_OLS[i], ")\n")
  cat("  FGLS:", comparacao$FGLS[i], " (Erro:", comparacao$Erro_FGLS[i], ")\n")
  
  if(comparacao$Erro_FGLS[i] < comparacao$Erro_OLS[i]) {
    cat("  ✅ FGLS MELHOR que OLS\n")
  } else {
    cat("  ❌ FGLS PIOR que OLS\n")
  }
  cat("\n")
}

cat("CONCLUSÃO GERAL:\n")
cat("----------------\n")
if(melhoria > 0) {
  cat("✅ FGLS PRODUZIU ESTIMATIVAS MAIS PRÓXIMAS DOS VERDADEIROS\n")
  cat("   Melhoria média de", round(melhoria, 1), "% em relação ao OLS\n")
  cat("   FGLS é mais eficiente na presença de heterocedasticidade\n")
} else {
  cat("❌ FGLS NÃO MELHOROU as estimativas em relação ao OLS\n")
  cat("   Possíveis causas: amostra pequena ou especificação incorreta da variância\n")
}

cat("\nRESULTADO ESPERADO:\n")
cat("-------------------\n")
cat("Em teoria, FGLS deveria produzir estimativas mais eficientes\n")
cat("(menor variância) que OLS na presença de heterocedasticidade\n")

sink()

# --- MENSAGEM FINAL ---
cat("\n✅ QUESTÃO 1.11 CONCLUÍDA!\n")
cat("=======================\n")
cat("📊 RESULTADOS SALVOS:\n")
cat("   • comparacao_fgls_1.11.html (Tabela formatada)\n")
cat("   • relatorio_detalhado_1.11.txt (Análise detalhada)\n\n")

cat("🎯 RESULTADO DA COMPARAÇÃO:\n")
cat("   Erro médio OLS:", round(erro_medio_ols, 4), "\n")
cat("   Erro médio FGLS:", round(erro_medio_fgls, 4), "\n")
cat("   Melhoria do FGLS:", round(melhoria, 1), "%\n")
cat("   Conclusão:", ifelse(melhoria > 0, 
                            "✅ FGLS MELHOR que OLS",
                            "❌ FGLS NÃO melhorou"), "\n")

cat("\n📈 PARA VISUALIZAR OS RESULTADOS:\n")
cat("browseURL('output/comparacao_fgls_1.11.html')\n")

