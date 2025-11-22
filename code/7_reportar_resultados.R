source("code/_master.R")

cat("Reportando resultados da regressão 1.4 com stargazer...\n")

# --- CARREGAR DADOS E REESTIMAR MODELO ---
dados <- read.csv(file.path(tmp_dir, "dados_simulados.csv"))

# Preparar dados com dplyr
dados_preparados <- dados %>%
  mutate(experience_sq = experience^2) %>%
  select(log_wage_homo, education, experience, experience_sq,
         north, northeast, south, centerwest, male, white)

# Reestimando o modelo para garantir que está disponível
modelo_mincer <- lm(log_wage_homo ~ education + experience + experience_sq +
                      north + northeast + south + centerwest + male + white,
                    data = dados_preparados)

# --- RELATÓRIO 1: TABELA FORMATADA BÁSICA ---
cat("Gerando tabela formatada básica...\n")
stargazer(
  modelo_mincer,
  type = "html",
  title = "Tabela 1: Determinantes do Log do Salário - Equação de Mincer",
  dep.var.labels = "Log do Salário",
  covariate.labels = c(
    "Anos de Estudo",
    "Experiência Profissional",
    "Experiência ao Quadrado",
    "Região Norte",
    "Região Nordeste",
    "Região Sul", 
    "Região Centro-Oeste",
    "Sexo Masculino",
    "Raça Branca",
    "Constante"
  ),
  digits = 3,
  align = TRUE,
  no.space = TRUE,
  out = file.path(output_dir, "resultado_regressao_formatado.html")
)

# --- RELATÓRIO 2: TABELA COMPLETA COM ESTATÍSTICAS ---
cat("Gerando tabela completa com estatísticas...\n")
stargazer(
  modelo_mincer,
  type = "html", 
  title = "Tabela 2: Resultados Completos da Regressão - Equação de Mincer",
  dep.var.labels = "Log do Salário",
  covariate.labels = c(
    "Anos de Estudo",
    "Experiência Profissional", 
    "Experiência ao Quadrado",
    "Região Norte",
    "Região Nordeste",
    "Região Sul",
    "Região Centro-Oeste", 
    "Sexo Masculino",
    "Raça Branca",
    "Constante"
  ),
  digits = 4,
  align = TRUE,
  no.space = TRUE,
  omit.stat = c("LL", "ser", "f"),
  add.lines = list(
    c("Observações", nrow(dados_preparados)),
    c("R²", round(summary(modelo_mincer)$r.squared, 4)),
    c("R² Ajustado", round(summary(modelo_mincer)$adj.r.squared, 4))
  ),
  out = file.path(output_dir, "resultado_completo.html")
)

# --- RELATÓRIO 3: VERSÃO SIMPLIFICADA PARA ANÁLISE ---
cat("Gerando versão simplificada para análise...\n")
stargazer(
  modelo_mincer,
  type = "html",
  title = "Tabela 3: Impactos Percentuais no Salário - Equação de Mincer",
  dep.var.caption = "Variável Dependente: Log do Salário",
  dep.var.labels = "",
  covariate.labels = c(
    "Anos de Estudo",
    "Experiência Profissional",
    "Experiência ao Quadrado", 
    "Região Norte",
    "Região Nordeste",
    "Região Sul",
    "Região Centro-Oeste",
    "Sexo Masculino",
    "Raça Branca",
    "Constante"
  ),
  digits = 4,
  notes = c(
    "Nota 1: Coeficientes representam efeitos no log do salário.",
    "Nota 2: Efeito percentual aproximado = (exp(β)-1)*100%.",
    "Nota 3: Região de referência: Sudeste."
  ),
  notes.append = FALSE,
  out = file.path(output_dir, "resultado_analise.html")
)

# --- EXIBIR RESUMO NO CONSOLE ---
cat("\n📊 RESUMO DOS RESULTADOS - MODELO MINCER\n")
cat("========================================\n")
cat("Observações:", nrow(dados_preparados), "\n")
cat("R-quadrado:", round(summary(modelo_mincer)$r.squared, 4), "\n")
cat("R-quadrado ajustado:", round(summary(modelo_mincer)$adj.r.squared, 4), "\n\n")

cat("PRINCIPAIS COEFICIENTES:\n")
cat("------------------------\n")
coefs <- coef(modelo_mincer)
cat("Educação:", round(coefs["education"], 4), 
    "(≈", round((exp(coefs["education"])-1)*100, 2), "% por ano de estudo)\n")
cat("Experiência:", round(coefs["experience"], 4), "\n")
cat("Experiência²:", round(coefs["experience_sq"], 6), "\n")
cat("Sexo Masculino:", round(coefs["male"], 4), 
    "(≈", round((exp(coefs["male"])-1)*100, 2), "% premium)\n")
cat("Raça Branca:", round(coefs["white"], 4), 
    "(≈", round((exp(coefs["white"])-1)*100, 2), "% premium)\n")

# --- MENSAGEM FINAL ---
cat("\n✅ RELATÓRIOS GERADOS COM SUCESSO!\n")
cat("📋 Arquivos criados na pasta output/:\n")
cat("   • resultado_regressao_formatado.html (tabela básica)\n")
cat("   • resultado_completo.html (tabela com estatísticas)\n")
cat("   • resultado_analise.html (versão para análise)\n\n")

cat("🎯 PARA VISUALIZAR OS RESULTADOS:\n")
cat("browseURL('output/resultado_regressao_formatado.html')\n")

