source("code/_master.R")

cat("Iniciando análise de resíduos...\n")

# --- CARREGAR DADOS PREDITOS ---
if (file.exists(file.path(tmp_dir, "dados_preditos.csv"))) {
  dados <- read.csv(file.path(tmp_dir, "dados_preditos.csv"))
  cat("✅ Dados preditos carregados\n")
} else {
  # Se não existir, carregar e calcular novamente
  cat("Recalculando dados preditos...\n")
  dados_originais <- read.csv(file.path(tmp_dir, "dados_simulados.csv"))
  
  dados_preparados <- dados_originais %>%
    mutate(experience_sq = experience^2) %>%
    select(log_wage_homo, education, experience, experience_sq,
           north, northeast, south, centerwest, male, white)
  
  modelo_mincer <- lm(log_wage_homo ~ education + experience + experience_sq +
                        north + northeast + south + centerwest + male + white,
                      data = dados_preparados)
  
  dados_preparados$log_wage_predito <- predict(modelo_mincer)
  dados <- dados_preparados
}

# --- CALCULAR RESÍDUOS ---
cat("Calculando resíduos...\n")
dados$residuos <- dados$log_wage_homo - dados$log_wage_predito
dados$residuos_quadrado <- dados$residuos^2

# --- ANÁLISE DESCRITIVA DOS RESÍDUOS ---
cat("Analisando distribuição dos resíduos...\n")
resumo_residuos <- list(
  media = mean(dados$residuos),
  mediana = median(dados$residuos),
  desvio_padrao = sd(dados$residuos),
  min = min(dados$residuos),
  max = max(dados$residuos),
  variancia = var(dados$residuos)
)

# --- CRIAR HISTOGRAMA DOS RESÍDUOS ---
cat("Criando histograma dos resíduos...\n")
histograma_residuos <- ggplot(dados, aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30, 
                 fill = "lightblue", 
                 color = "black",
                 alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(xintercept = 0, color = "darkgreen", linewidth = 1, linetype = "dashed") +
  labs(
    title = "Histograma dos Resíduos do Modelo de Mincer",
    subtitle = "Distribuição dos erros da regressão",
    x = "Resíduos (Log Salário Observado - Predito)",
    y = "Densidade",
    caption = paste("Média:", round(resumo_residuos$media, 4), 
                    "| Desvio Padrão:", round(resumo_residuos$desvio_padrao, 4))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic")
  )

# --- CRIAR HISTOGRAMA DOS RESÍDUOS AO QUADRADO ---
cat("Criando histograma dos resíduos ao quadrado...\n")
histograma_quadrado <- ggplot(dados, aes(x = residuos_quadrado)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30, 
                 fill = "lightcoral", 
                 color = "black",
                 alpha = 0.7) +
  geom_density(color = "darkred", linewidth = 1) +
  labs(
    title = "Histograma dos Resíduos ao Quadrado",
    subtitle = "Distribuição da magnitude dos erros",
    x = "Resíduos ao Quadrado",
    y = "Densidade",
    caption = paste("Variância dos resíduos:", round(resumo_residuos$variancia, 4))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic")
  )

# --- SALVAR GRÁFICOS ---
cat("Salvando histogramas...\n")
ggsave(
  filename = file.path(output_dir, "histograma_residuos.png"),
  plot = histograma_residuos,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "histograma_residuos_quadrado.png"),
  plot = histograma_quadrado,
  width = 10,
  height = 6,
  dpi = 300
)

# --- SALVAR RELATÓRIO DE DIAGNÓSTICO ---
cat("Gerando relatório de diagnóstico...\n")
sink(file.path(output_dir, "diagnostico_residuos.txt"))

cat("DIAGNÓSTICO DOS RESÍDUOS - MODELO MINCER\n")
cat("========================================\n")
cat("Data:", format(Sys.Date(), "%d/%m/%Y"), "\n\n")

cat("ESTATÍSTICAS DOS RESÍDUOS:\n")
cat("Média:", round(resumo_residuos$media, 6), "(deve ser próxima de 0)\n")
cat("Mediana:", round(resumo_residuos$mediana, 6), "\n")
cat("Desvio Padrão:", round(resumo_residuos$desvio_padrao, 6), "\n")
cat("Variância:", round(resumo_residuos$variancia, 6), "\n")
cat("Mínimo:", round(resumo_residuos$min, 6), "\n")
cat("Máximo:", round(resumo_residuos$max, 6), "\n\n")

cat("INTERPRETAÇÃO:\n")
cat("• Resíduos com média próxima de 0: BOA (modelo não tendencioso)\n")
cat("• Distribuição simétrica: BOA (suposição de normalidade)\n")
cat("• Resíduos grandes ao quadrado: indicam observações influentes\n")
cat("• Padrão sistemático nos resíduos: pode indicar má especificação\n")

sink()

# --- MENSAGEM FINAL ---
cat("\n✅ QUESTÃO 1.7 CONCLUÍDA!\n")
cat("📊 Histograma dos resíduos: output/histograma_residuos.png\n")
cat("📈 Histograma dos resíduos²: output/histograma_residuos_quadrado.png\n")
cat("📋 Diagnóstico: output/diagnostico_residuos.txt\n")
cat("📝 Resumo dos resíduos:\n")
print(unlist(resumo_residuos))

