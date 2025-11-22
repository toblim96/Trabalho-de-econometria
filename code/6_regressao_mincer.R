source("code/_master.R")

cat("Iniciando estimação do modelo de Mincer...\n")

# --- CARREGAR DADOS ---
dados <- read.csv(file.path(tmp_dir, "dados_simulados.csv"))
cat("✅ Dados carregados:", nrow(dados), "observações\n")

# --- PREPARAR VARIÁVEIS COM DPLYR ---
cat("Preparando variáveis...\n")
dados_preparados <- dados %>%
  # Criar quadrado da experiência (usando dplyr)
  mutate(experience_sq = experience^2) %>%
  # Selecionar apenas as variáveis necessárias
  select(log_wage_homo, education, experience, experience_sq,
         north, northeast, south, centerwest, male, white)

# --- RODAR REGRESSÃO LINEAR MÚLTIPLA ---
cat("Estimando modelo de regressão...\n")
modelo_mincer <- lm(log_wage_homo ~ education + experience + experience_sq +
                      north + northeast + south + centerwest + male + white,
                    data = dados_preparados)

# --- SALVAR RESULTADOS EM HTML ---
cat("Gerando tabela de resultados...\n")
stargazer(
  modelo_mincer,
  type = "html",
  title = "Equação de Mincer - Determinantes do Log do Salário",
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
  dep.var.labels = "Log do Salário",
  out = file.path(output_dir, "modelo_mincer.html")
)

# --- SALVAR RELATÓRIO DETALHADO EM TEXTO ---
cat("Gerando relatório detalhado...\n")
sink(file.path(output_dir, "resultados_mincer.txt"))

cat("RESULTADOS DA EQUAÇÃO DE MINCER\n")
cat("================================\n")
cat("Data:", format(Sys.Date(), "%d/%m/%Y"), "\n")
cat("Observações:", nrow(dados_preparados), "\n\n")

cat("ESPECIFICAÇÃO DO MODELO:\n")
cat("Variável Dependente: Log do Salário\n")
cat("Variáveis Independentes:\n")
cat("  - Anos de estudo\n")
cat("  - Experiência profissional\n")
cat("  - Experiência ao quadrado\n")
cat("  - Dummies regionais (referência: Sudeste)\n")
cat("  - Dummy de sexo (referência: Feminino)\n")
cat("  - Dummy de raça (referência: Não branco)\n\n")

cat("ESTATÍSTICAS DO MODELO:\n")
cat("R-quadrado:", round(summary(modelo_mincer)$r.squared, 4), "\n")
cat("R-quadrado ajustado:", round(summary(modelo_mincer)$adj.r.squared, 4), "\n")
cat("Estatística F:", round(summary(modelo_mincer)$fstatistic[1], 2), "\n\n")

cat("INTERPRETAÇÃO DOS COEFICIENTES:\n")
cat("================================\n")
coeficientes <- coef(modelo_mincer)
cat("Retorno da educação:", round(coeficientes["education"], 4), 
    "(cada ano adicional de estudo aumenta o salário em", 
    round(exp(coeficientes["education"])*100-100, 2), "%)\n")

cat("Retorno da experiência:", round(coeficientes["experience"], 4), "\n")
cat("Experiência ao quadrado:", round(coeficientes["experience_sq"], 6), 
    "(indica retornos decrescentes)\n")

cat("Prêmio salarial masculino:", round(coeficientes["male"], 4),
    "(homens ganham", round(exp(coeficientes["male"])*100-100, 2), "% mais)\n")

cat("Prêmio salarial brancos:", round(coeficientes["white"], 4),
    "(brancos ganham", round(exp(coeficientes["white"])*100-100, 2), "% mais)\n\n")

cat("EFEITOS REGIONAIS (vs. SUDESTE):\n")
cat("Norte:", round(coeficientes["north"], 4), "\n")
cat("Nordeste:", round(coeficientes["northeast"], 4), "\n")
cat("Sul:", round(coeficientes["south"], 4), "\n")
cat("Centro-Oeste:", round(coeficientes["centerwest"], 4), "\n")

sink()

# --- MENSAGEM FINAL ---
cat("\n✅ QUESTÃO 1.4 CONCLUÍDA!\n")
cat("📊 Tabela do modelo: output/modelo_mincer.html\n")
cat("📋 Relatório detalhado: output/resultados_mincer.txt\n")
cat("🔍 Resumo estatístico do modelo:\n")
print(summary(modelo_mincer))

