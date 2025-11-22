# ================================================================
# ARQUIVO: 15_questao_1.8.R
# TRABALHO: Econometria 1
# PROFESSOR: Vitor Pereira
# GRUPO: 1
# OBJETIVO: Refazer questões 1.4 a 1.7 com log_wage_hetero e analisar mudanças
# INPUT: dados_simulados.csv (pasta tmp/)
# OUTPUT:
#   - modelo_mincer_hetero.html (1.4)
#   - resultado_hetero_formatado.html (1.5)
#   - grafico_3d_hetero.html (1.6)
#   - analise_residuos_hetero.png (1.7)
#   - comparacao_residuos_educacao.png (RESPOSTA PRINCIPAL)
# DATA: Novembro de 2024
# ================================================================

# --- CARREGAR CONFIGURAÇÕES E PACOTES ---
source("code/_master.R")

cat("INICIANDO QUESTÃO 1.8 - ANÁLISE COM SALÁRIOS HETEROCEDÁSTICOS\n")
cat("============================================================\n\n")

# --- INSTALAR/CARREGAR PACOTES ADICIONAIS ---
if (!require(plotly)) {
  install.packages("plotly")
  library(plotly)
}

if (!require(lmtest)) {
  install.packages("lmtest")
  library(lmtest)
}

# --- CARREGAR E PREPARAR DADOS (UMA ÚNICA VEZ) ---
cat("1. CARREGANDO E PREPARANDO DADOS...\n")
dados <- read.csv(file.path(tmp_dir, "dados_simulados.csv"))
cat("   ✅ Dados carregados:", nrow(dados), "observações\n")

# Preparar dados para ambos os modelos
dados_preparados <- dados %>%
  mutate(experience_sq = experience^2) %>%
  select(log_wage_homo, log_wage_hetero, education, experience, experience_sq,
         north, northeast, south, centerwest, male, white)

# ================================================================
# QUESTÃO 1.4: ESTIMAR MODELO COM log_wage_hetero
# ================================================================
cat("\n2. QUESTÃO 1.4: ESTIMANDO MODELO HETEROCEDÁSTICO...\n")

modelo_mincer_hetero <- lm(log_wage_hetero ~ education + experience + experience_sq +
                             north + northeast + south + centerwest + male + white,
                           data = dados_preparados)

# Modelo homocedástico para comparação
modelo_mincer_homo <- lm(log_wage_homo ~ education + experience + experience_sq +
                           north + northeast + south + centerwest + male + white,
                         data = dados_preparados)

cat("   ✅ Modelos estimados com sucesso\n")

# ================================================================
# QUESTÃO 1.5: REPORTAR RESULTADOS COM STARGAZER
# ================================================================
cat("\n3. QUESTÃO 1.5: REPORTANDO RESULTADOS...\n")

# Tabela do modelo heterocedástico
stargazer(
  modelo_mincer_hetero,
  type = "html",
  title = "Equação de Mincer - Salários HETEROCEDÁSTICOS (Questão 1.4)",
  covariate.labels = c(
    "Anos de Estudo", "Experiência", "Experiência²",
    "Norte", "Nordeste", "Sul", "Centro-Oeste",
    "Masculino", "Branco", "Constante"
  ),
  dep.var.labels = "Log do Salário Heterocedástico",
  out = file.path(output_dir, "modelo_mincer_hetero.html")
)

# Tabela comparativa
stargazer(
  list(modelo_mincer_homo, modelo_mincer_hetero),
  type = "html",
  title = "Comparação: Modelos Homocedástico vs Heterocedástico (Questão 1.5)",
  column.labels = c("Homocedástico", "Heterocedástico"),
  covariate.labels = c(
    "Anos de Estudo", "Experiência", "Experiência²",
    "Norte", "Nordeste", "Sul", "Centro-Oeste",
    "Masculino", "Branco", "Constante"
  ),
  dep.var.labels = "Log do Salário",
  out = file.path(output_dir, "resultado_comparativo_1.5.html")
)

cat("   ✅ Resultados reportados em HTML\n")

# ================================================================
# QUESTÃO 1.6: GRÁFICO 3D COM VALORES PREDITOS
# ================================================================
cat("\n4. QUESTÃO 1.6: CRIANDO GRÁFICO 3D...\n")

# Calcular valores preditos
dados_preparados$log_wage_predito_hetero <- predict(modelo_mincer_hetero)
dados_preparados$log_wage_predito_homo <- predict(modelo_mincer_homo)

# Salvar dados com predições
write.csv(dados_preparados, file.path(tmp_dir, "dados_completos_preditos.csv"), row.names = FALSE)

# Criar gráfico 3D interativo
grafico_3d_hetero <- plot_ly() %>%
  add_trace(
    data = dados_preparados,
    x = ~experience, y = ~education, z = ~log_wage_hetero,
    type = "scatter3d", mode = "markers",
    marker = list(size = 3, color = ~log_wage_hetero, colorscale = "Viridis", opacity = 0.7),
    name = "Dados Heterocedásticos"
  ) %>%
  add_trace(
    data = dados_preparados,
    x = ~experience, y = ~education, z = ~log_wage_predito_hetero,
    type = "mesh3d", intensity = ~log_wage_predito_hetero, colorscale = "Hot", opacity = 0.7,
    name = "Superfície Preditiva"
  ) %>%
  layout(
    title = "Gráfico 3D: Log do Salário HETEROCEDÁSTICO (Questão 1.6)",
    scene = list(
      xaxis = list(title = "Experiência (anos)"),
      yaxis = list(title = "Educação (anos)"), 
      zaxis = list(title = "Log do Salário Heterocedástico")
    )
  )

# Salvar gráfico 3D
htmlwidgets::saveWidget(
  widget = grafico_3d_hetero,
  file = file.path(output_dir, "grafico_3d_hetero_1.6.html"),
  selfcontained = TRUE
)

cat("   ✅ Gráfico 3D interativo salvo\n")

# ================================================================
# QUESTÃO 1.7: ANÁLISE DOS RESÍDUOS E RESPOSTA PRINCIPAL
# ================================================================
cat("\n5. QUESTÃO 1.7: ANALISANDO RESÍDUOS...\n")

# Calcular resíduos para ambos os modelos
dados_preparados$residuos_hetero <- residuals(modelo_mincer_hetero)
dados_preparados$residuos_homo <- residuals(modelo_mincer_homo)
dados_preparados$residuos_hetero_quadrado <- dados_preparados$residuos_hetero^2
dados_preparados$residuos_homo_quadrado <- dados_preparados$residuos_homo^2

# --- GRÁFICO 1: HISTOGRAMA DOS RESÍDUOS HETEROCEDÁSTICOS ---
histograma_hetero <- ggplot(dados_preparados, aes(x = residuos_hetero)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightcoral", color = "darkred", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1, linetype = "dashed") +
  labs(
    title = "Histograma dos Resíduos - Modelo HETEROCEDÁSTICO (Questão 1.7)",
    x = "Resíduos", y = "Densidade"
  ) +
  theme_minimal()

# --- GRÁFICO 2: RESPOSTA PRINCIPAL - COMPARAÇÃO RESÍDUOS vs EDUCAÇÃO ---
dados_comparacao <- data.frame(
  education = rep(dados_preparados$education, 2),
  residuos = c(dados_preparados$residuos_hetero, dados_preparados$residuos_homo),
  modelo = rep(c("HETEROCEDÁSTICO", "HOMOCEDÁSTICO"), each = nrow(dados_preparados))
)

comparacao_residuos <- ggplot(dados_comparacao, aes(x = education, y = residuos, color = modelo)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  facet_wrap(~ modelo, ncol = 2) +
  labs(
    title = "RESPOSTA QUESTÃO 1.8: Resíduos vs Educação - Heterocedástico vs Homocedástico",
    subtitle = "MUDANÇA PRINCIPAL: Aparecimento de PADRÃO DE FUNIL (heterocedasticidade)",
    x = "Anos de Estudo", y = "Resíduos", color = "Modelo"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("HETEROCEDÁSTICO" = "red", "HOMOCEDÁSTICO" = "blue"))

# --- GRÁFICO 3: RESÍDUOS AO QUADRADO ---
residuos_quadrado <- ggplot(dados_preparados, aes(x = residuos_hetero_quadrado)) +
  geom_histogram(bins = 30, fill = "orange", color = "darkorange", alpha = 0.7) +
  labs(
    title = "Resíduos ao Quadrado - Modelo Heterocedástico (Questão 1.7)",
    x = "Resíduos ao Quadrado", y = "Frequência"
  ) +
  theme_minimal()

# Salvar todos os gráficos
ggsave(file.path(output_dir, "histograma_residuos_hetero_1.7.png"), histograma_hetero, width = 10, height = 6, dpi = 300)
ggsave(file.path(output_dir, "comparacao_residuos_educacao_1.8.png"), comparacao_residuos, width = 12, height = 6, dpi = 300)
ggsave(file.path(output_dir, "residuos_quadrado_1.7.png"), residuos_quadrado, width = 10, height = 6, dpi = 300)

# ================================================================
# ANÁLISE ESTATÍSTICA E RELATÓRIO FINAL
# ================================================================
cat("\n6. ANÁLISE ESTATÍSTICA E RELATÓRIO FINAL...\n")

# Testes de heterocedasticidade
teste_bp_hetero <- bptest(modelo_mincer_hetero)
teste_bp_homo <- bptest(modelo_mincer_homo)

# Salvar relatório completo
sink(file.path(output_dir, "relatorio_final_1.8.txt"))

cat("RELATÓRIO FINAL - QUESTÃO 1.8\n")
cat("=============================\n\n")

cat("RESPOSTA À PERGUNTA PRINCIPAL:\n")
cat("'O QUE MUDOU NO GRÁFICO DOS RESÍDUOS CONTRA ANOS DE ESTUDO?'\n")
cat("------------------------------------------------------------\n\n")

cat("📊 MUDANÇA OBSERVADA:\n")
cat("   • MODELO HOMOCEDÁSTICO: Resíduos com variância constante\n")
cat("   • MODELO HETEROCEDÁSTICO: PADRÃO DE FUNIL - variância aumenta com educação\n")
cat("   • Isso caracteriza HETEROCEDASTICIDADE\n\n")

cat("📈 RESULTADOS DOS TESTES ESTATÍSTICOS:\n")
cat("Teste de Breusch-Pagan - Modelo Heterocedástico:\n")
cat("   p-value =", round(teste_bp_hetero$p.value, 6), "\n")
cat("   Conclusão:", ifelse(teste_bp_hetero$p.value < 0.05, 
                            "✅ REJEITA homocedasticidade (heterocedasticidade presente)",
                            "✅ NÃO REJEITA homocedasticidade"), "\n\n")

cat("Teste de Breusch-Pagan - Modelo Homocedástico:\n")
cat("   p-value =", round(teste_bp_homo$p.value, 6), "\n")
cat("   Conclusão:", ifelse(teste_bp_homo$p.value < 0.05, 
                            "✅ REJEITA homocedasticidade",
                            "✅ NÃO REJEITA homocedasticidade"), "\n\n")

cat("🎯 INTERPRETAÇÃO ECONÔMICA:\n")
cat("A heterocedasticidade reflete que:\n")
cat("• Educação superior oferece trajetórias profissionais mais diversificadas\n")
cat("• Salários tornam-se mais dispersos com maior qualificação\n")
cat("• Pessoas com mesma educação podem ter remunerações muito diferentes\n")
cat("• Isso é comum em mercados de trabalho reais\n")

cat("\n📁 ARQUIVOS GERADOS:\n")
cat("• modelo_mincer_hetero.html (Questão 1.4)\n")
cat("• resultado_comparativo_1.5.html (Questão 1.5)\n")
cat("• grafico_3d_hetero_1.6.html (Questão 1.6)\n")
cat("• comparacao_residuos_educacao_1.8.png (RESPOSTA PRINCIPAL)\n")
cat("• histograma_residuos_hetero_1.7.png (Questão 1.7)\n")
cat("• residuos_quadrado_1.7.png (Questão 1.7)\n")

sink()

# ================================================================
# MENSAGEM FINAL
# ================================================================
cat("\n✅ QUESTÃO 1.8 COMPLETAMENTE RESOLVIDA!\n")
cat("======================================\n")
cat("📊 RESULTADOS SALVOS NA PASTA output/:\n")
cat("   1.4 - modelo_mincer_hetero.html\n")
cat("   1.5 - resultado_comparativo_1.5.html\n")
cat("   1.6 - grafico_3d_hetero_1.6.html\n")
cat("   1.7 - histograma_residuos_hetero_1.7.png\n")
cat("   🎯 RESPOSTA - comparacao_residuos_educacao_1.8.png\n\n")

cat("🔍 PARA VER A RESPOSTA PRINCIPAL:\n")
cat("browseURL('output/comparacao_residuos_educacao_1.8.png')\n\n")

cat("📈 O QUE MUDOU NOS RESÍDUOS?\n")
cat("PADRÃO DE FUNIL: Variância aumenta com educação → HETEROCEDASTICIDADE\n")

