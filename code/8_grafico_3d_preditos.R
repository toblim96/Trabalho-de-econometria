source("code/_master.R")

cat("Iniciando criação do gráfico 3D...\n")

# --- INSTALAR/CARREGAR PLOTLY ---
if (!require(plotly)) {
  install.packages("plotly")
  library(plotly)
}

# --- CARREGAR DADOS E MODELO ---
dados <- read.csv(file.path(tmp_dir, "dados_simulados.csv"))

# Preparar dados (igual na questão 1.4)
dados_preparados <- dados %>%
  mutate(experience_sq = experience^2) %>%
  select(log_wage_homo, education, experience, experience_sq,
         north, northeast, south, centerwest, male, white)

# Reestimando o modelo para garantir disponibilidade
modelo_mincer <- lm(log_wage_homo ~ education + experience + experience_sq +
                      north + northeast + south + centerwest + male + white,
                    data = dados_preparados)

# --- CALCULAR VALORES PREDITOS ---
cat("Calculando valores preditos...\n")
dados_preparados$log_wage_predito <- predict(modelo_mincer)

# Salvar dados com valores preditos
write.csv(dados_preparados, file.path(tmp_dir, "dados_preditos.csv"), row.names = FALSE)

# --- CRIAR GRÁFICO 3D ---
cat("Criando gráfico 3D interativo...\n")
grafico_3d <- plot_ly() %>%
  
  # Adicionar pontos observados
  add_trace(
    data = dados_preparados,
    x = ~experience, 
    y = ~education,
    z = ~log_wage_homo,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 3,
      color = ~log_wage_homo,
      colorscale = "Viridis",
      opacity = 0.7
    ),
    name = "Dados Observados"
  ) %>%
  
  # Adicionar superfície predita
  add_trace(
    data = dados_preparados,
    x = ~experience,
    y = ~education, 
    z = ~log_wage_predito,
    type = "mesh3d",
    intensity = ~log_wage_predito,
    colorscale = "Hot",
    opacity = 0.7,
    name = "Superfície Preditiva"
  ) %>%
  
  # Configurar layout
  layout(
    title = list(
      text = "Gráfico 3D: Log do Salário vs Experiência e Educação",
      font = list(size = 16)
    ),
    scene = list(
      xaxis = list(title = "Experiência (anos)"),
      yaxis = list(title = "Educação (anos de estudo)"), 
      zaxis = list(title = "Log do Salário"),
      camera = list(
        eye = list(x = 1.5, y = 1.5, z = 1.5)
      )
    ),
    legend = list(
      x = 0,
      y = 1,
      bgcolor = "lightgray"
    )
  )

# --- SALVAR GRÁFICO INTERATIVO ---
cat("Salvando gráfico 3D interativo...\n")
htmlwidgets::saveWidget(
  widget = grafico_3d,
  file = file.path(output_dir, "grafico_3d_salario.html"),
  selfcontained = TRUE
)

# --- MENSAGEM FINAL ---
cat("\n✅ QUESTÃO 1.6 CONCLUÍDA!\n")
cat("📊 Gráfico 3D salvo: output/grafico_3d_salario.html\n")
cat("💾 Dados preditos salvos: tmp/dados_preditos.csv\n")
cat("🎯 PARA VISUALIZAR: Abra o arquivo HTML no navegador\n")
cat("📍 O gráfico é INTERATIVO - você pode rotacionar e zoom!\n")

