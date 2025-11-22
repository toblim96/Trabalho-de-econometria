# ================================================================
# ARQUIVO: 17_questao_1.9.R
# TRABALHO: Econometria 1
# PROFESSOR: Vitor Pereira
# GRUPO: 1
# OBJETIVO: Teste de White para heterocedasticidade em log_wage_homo
# INPUT: dados_simulados.csv (pasta tmp/)
# OUTPUT:
#   - teste_white_homo.html (pasta output/)
#   - relatorio_white_homo.txt (pasta output/)
# DATA: Novembro de 2024
# ================================================================

# --- CARREGAR CONFIGURAÇÕES ---
source("code/_master.R")

cat("QUESTÃO 1.9: TESTE DE WHITE PARA log_wage_homo\n")
cat("==============================================\n\n")

# --- EXPLICAÇÃO DA QUESTÃO ---
cat("OBJETIVO: Testar presença de heterocedasticidade na regressão com log_wage_homo\n")
cat("MÉTODO: Teste de White usando fitted values e seus quadrados\n")
cat("HIPÓTESE NULA (H0): Homocedasticidade (variância constante)\n")
cat("HIPÓTESE ALTERNATIVA (H1): Heterocedasticidade (variância não constante)\n\n")

# --- CARREGAR DADOS ---
cat("1. CARREGANDO DADOS...\n")
dados <- read.csv(file.path(tmp_dir, "dados_simulados.csv"))

# Preparar dados
dados_preparados <- dados %>%
  mutate(experience_sq = experience^2) %>%
  select(log_wage_homo, education, experience, experience_sq,
         north, northeast, south, centerwest, male, white)

cat("   ✅ Dados carregados:", nrow(dados_preparados), "observações\n")

# --- ESTIMAR MODELO COM log_wage_homo ---
cat("\n2. ESTIMANDO MODELO COM log_wage_homo...\n")
modelo_homo <- lm(log_wage_homo ~ education + experience + experience_sq +
                    north + northeast + south + centerwest + male + white,
                  data = dados_preparados)

cat("   ✅ Modelo estimado:\n")
cat("   - R²:", round(summary(modelo_homo)$r.squared, 4), "\n")
cat("   - Observações:", nrow(dados_preparados), "\n")

# --- REALIZAR TESTE DE WHITE ---
cat("\n3. REALIZANDO TESTE DE WHITE...\n")
cat("   Especificação do teste: ~ fitted(m) + I(fitted(m)^2)\n")

teste_white_homo <- bptest(modelo_homo, 
                           ~ fitted(modelo_homo) + I(fitted(modelo_homo)^2))

# --- SALVAR RESULTADOS EM HTML ---
cat("\n4. REPORTANDO RESULTADOS COM STARGAZER...\n")

# Criar tabela de resultados
resultado_teste <- data.frame(
  Descricao = c(
    "Variável Dependente",
    "Estatística do Teste", 
    "p-value",
    "Nível de Significância",
    "Conclusão"
  ),
  Valor = c(
    "log_wage_homo",
    round(teste_white_homo$statistic, 4),
    round(teste_white_homo$p.value, 6),
    "5%",
    ifelse(teste_white_homo$p.value < 0.05, 
           "REJEITA H0 - Heterocedasticidade presente",
           "NÃO REJEITA H0 - Homocedasticidade")
  )
)

stargazer(
  resultado_teste,
  type = "html",
  title = "QUESTÃO 1.9: Resultado do Teste de White para log_wage_homo",
  summary = FALSE,
  rownames = FALSE,
  out = file.path(output_dir, "teste_white_homo_1.9.html")
)

# --- SALVAR RELATÓRIO DETALHADO ---
cat("\n5. GERANDO RELATÓRIO DETALHADO...\n")
sink(file.path(output_dir, "relatorio_detalhado_1.9.txt"))

cat("RELATÓRIO DETALHADO - QUESTÃO 1.9\n")
cat("=================================\n\n")

cat("TESTE DE WHITE PARA DETECÇÃO DE HETEROCEDASTICIDADE\n")
cat("--------------------------------------------------\n\n")

cat("ESPECIFICAÇÃO DO MODELO:\n")
cat("Variável Dependente: log_wage_homo\n")
cat("Variáveis Independentes: education, experience, experience_sq,\n")
cat("                         north, northeast, south, centerwest, male, white\n\n")

cat("ESPECIFICAÇÃO DO TESTE DE WHITE:\n")
cat("bptest(modelo, ~ fitted(modelo) + I(fitted(modelo)^2))\n\n")

cat("RESULTADOS DO TESTE:\n")
cat("--------------------\n")
cat("Estatística LM:", round(teste_white_homo$statistic, 4), "\n")
cat("Graus de liberdade: 2\n")
cat("p-value:", round(teste_white_homo$p.value, 6), "\n\n")

cat("INTERPRETAÇÃO:\n")
cat("--------------\n")
cat("Hipótese Nula (H0): Homocedasticidade\n")
cat("Hipótese Alternativa (H1): Heterocedasticidade\n\n")

if (teste_white_homo$p.value < 0.05) {
  cat("CONCLUSÃO: REJEITAMOS H0 ao nível de 5% de significância\n")
  cat("           → Há evidências de HETEROCEDASTICIDADE\n")
  cat("           → A variância dos erros NÃO é constante\n")
} else {
  cat("CONCLUSÃO: NÃO REJEITAMOS H0 ao nível de 5% de significância\n")
  cat("           → NÃO há evidências de heterocedasticidade\n")
  cat("           → A variância dos erros é CONSTANTE (homocedasticidade)\n")
}

cat("\nRESULTADO ESPERADO:\n")
cat("-------------------\n")
cat("Como log_wage_homo foi gerado com erros homocedásticos,\n")
cat("espera-se NÃO rejeitar H0 (p-value > 0.05)\n")

sink()

# --- MENSAGEM FINAL ---
cat("\n✅ QUESTÃO 1.9 CONCLUÍDA!\n")
cat("=======================\n")
cat("📊 RESULTADOS SALVOS:\n")
cat("   • teste_white_homo_1.9.html (Tabela formatada)\n")
cat("   • relatorio_detalhado_1.9.txt (Análise detalhada)\n\n")

cat("🎯 RESULTADO DO TESTE:\n")
cat("   Estatística LM:", round(teste_white_homo$statistic, 4), "\n")
cat("   p-value:", round(teste_white_homo$p.value, 6), "\n")
cat("   Conclusão:", ifelse(teste_white_homo$p.value < 0.05, 
                            "✅ REJEITA H0 - Heterocedasticidade",
                            "✅ NÃO REJEITA H0 - Homocedasticidade"), "\n")

cat("\n📈 PARA VISUALIZAR OS RESULTADOS:\n")
cat("browseURL('output/teste_white_homo_1.9.html')\n")