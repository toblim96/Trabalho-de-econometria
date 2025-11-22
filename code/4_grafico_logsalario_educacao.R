source("code/_master.R")

cat("Iniciando análise: Log Salário vs Educação...\n")

# --- CARREGAR DADOS ---
dados <- read.csv(file.path(tmp_dir, "dados_simulados.csv"))
cat("✅ Dados carregados:", nrow(dados), "observações\n")

# --- RODAR REGRESSÃO LINEAR ---
cat("Rodando regressão linear...\n")
modelo_logsalario <- lm(log_wage_homo ~ education, data = dados)

# --- CRIAR GRÁFICO COM GGPLOT2 ---
cat("Criando scatterplot...\n")
grafico_log <- ggplot(dados, aes(x = education, y = log_wage_homo)) +
  geom_point(alpha = 0.6, color = "darkgreen", size = 1.5) +  # Pontos do scatterplot
  geom_smooth(method = "lm", color = "orange", se = TRUE, linewidth = 1) +  # Linha de regressão
  labs(
    title = "Relação entre Log do Salário e Educação",
    subtitle = "Scatterplot com Linha de Regressão Linear",
    x = "Anos de Estudo",
    y = "Log do Salário",
    caption = paste("Equação: Log(Salário) =", 
                    round(coef(modelo_logsalario)[1], 2), "+", 
                    round(coef(modelo_logsalario)[2], 2), "* Educação")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

# --- SALVAR GRÁFICO ---
ggsave(
  filename = file.path(output_dir, "grafico_logsalario_educacao.png"),
  plot = grafico_log,
  width = 10,
  height = 6,
  dpi = 300
)

# --- SALVAR RESUMO DO MODELO ---
stargazer(
  modelo_logsalario,
  type = "html",
  
  title = "Regressão Linear: Log do Salário vs Educação",
  out = file.path(output_dir, "modelo_logsalario_educacao.html")
)

# --- EXIBIR RESULTADOS NO CONSOLE ---
cat("\n✅ ANÁLISE CONCLUÍDA!\n")
cat("📈 Gráfico salvo: grafico_logsalario_educacao.png\n")
cat("📊 Tabela do modelo: modelo_logsalario_educacao.html\n")
cat("📋 Resumo da regressão:\n")
print(summary(modelo_logsalario))