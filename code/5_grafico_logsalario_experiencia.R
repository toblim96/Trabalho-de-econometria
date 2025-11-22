source("code/_master.R")

cat("Iniciando análise: Log Salário vs Experiência...\n")

# --- VERIFICAR SE GGPLOT2 ESTÁ CARREGADO ---
if (!"ggplot2" %in% loadedNamespaces()) {
  cat("❌ ggplot2 não carregado. Carregando agora...\n")
  library(ggplot2)
}

# --- CARREGAR DADOS ---
dados <- read.csv(file.path(tmp_dir, "dados_simulados.csv"))
cat("✅ Dados carregados:", nrow(dados), "observações\n")

# --- RODAR REGRESSÃO LINEAR ---
cat("Rodando regressão linear...\n")
modelo_experiencia <- lm(log_wage_homo ~ experience, data = dados)

# --- CRIAR GRÁFICO COM GGPLOT2 ---
cat("Criando scatterplot...\n")

grafico_experiencia <- ggplot(data = dados, aes(x = experience, y = log_wage_homo)) +
  geom_point(alpha = 0.6, color = "purple", size = 1.5) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE, linewidth = 1) +
  labs(
    title = "Relação entre Log do Salário e Experiência Profissional",
    subtitle = "Scatterplot com Linha de Regressão Linear",
    x = "Anos de Experiência Profissional",
    y = "Log do Salário",
    caption = paste("Equação: Log(Salário) =", 
                    round(coef(modelo_experiencia)[1], 2), "+", 
                    round(coef(modelo_experiencia)[2], 2), "* Experiência")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(face = "italic")
  )

# --- SALVAR GRÁFICO ---
cat("Salvando gráfico...\n")
ggsave(
  filename = file.path(output_dir, "grafico_logsalario_experiencia.png"),
  plot = grafico_experiencia,
  width = 10,
  height = 6,
  dpi = 300
)

# --- SALVAR RESUMO DO MODELO ---
stargazer(
  modelo_experiencia,
  type = "html",
  title = "Regressão Linear: Log do Salário vs Experiência Profissional",
  out = file.path(output_dir, "modelo_logsalario_experiencia.html")
)

# --- EXIBIR RESULTADOS ---
cat("\n✅ QUESTÃO 1.3 CONCLUÍDA!\n")
cat("📈 Gráfico salvo: output/grafico_logsalario_experiencia.png\n")
cat("📊 Tabela do modelo: output/modelo_logsalario_experiencia.html\n")
cat("📋 Resumo da regressão:\n")
cat("   - Coeficiente da experiência:", round(coef(modelo_experiencia)[2], 4), "\n")
cat("   - R-quadrado:", round(summary(modelo_experiencia)$r.squared, 4), "\n")
cat("   - Observações:", nrow(dados), "\n")

