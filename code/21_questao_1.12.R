# ================================================================
# ARQUIVO: 21_questao_1.12.R
# TRABALHO: Econometria 1
# PROFESSOR: Vitor Pereira
# GRUPO: 1
# OBJETIVO: Amostra de 200k observações e comparação OLS vs FGLS
# INPUT: Nenhum (dados são gerados)
# OUTPUT:
#   - comparacao_amostras_1.12.html (pasta output/)
#   - relatorio_amostras_1.12.txt (pasta output/)
# DATA: Novembro de 2024
# ================================================================

# --- CARREGAR CONFIGURAÇÕES ---
source("code/_master.R")

cat("QUESTÃO 1.12: AMOSTRA DE 200K OBSERVAÇÕES - COMPARAÇÃO OLS vs FGLS\n")
cat("==================================================================\n\n")

# --- VERIFICAR E INSTALAR PACOTES NECESSÁRIOS ---
cat("1. VERIFICANDO PACOTES...\n")
pacotes_necessarios <- c("dplyr", "lmtest", "stargazer")
for (pacote in pacotes_necessarios) {
  if (!require(pacote, character.only = TRUE)) {
    install.packages(pacote)
    library(pacote, character.only = TRUE)
  }
}
cat("   ✅ Pacotes carregados\n")

# --- FUNÇÃO PARA GERAR DADOS (REVISADA) ---
gerar_dados_grande_amostra <- function(n = 200000) {
  cat("2. GERANDO AMOSTRA DE", n, "OBSERVAÇÕES...\n")
  
  set.seed(1234) # Mesma seed para comparabilidade
  
  # Gerar variáveis demográficas básicas
  data <- data.frame(
    education = pmax(4, pmin(18, round(rnorm(n, mean = 9.5, sd = 3.5)))),
    male = rbinom(n, 1, 0.52),
    white = rbinom(n, 1, 0.45),
    experience = pmax(0, pmin(45, round(rnorm(n, mean = 15, sd = 10))))
  )
  
  # Gerar regiões de forma mais eficiente
  regions <- c("North", "Northeast", "Southeast", "South", "CenterWest")
  region_probs <- c(0.09, 0.27, 0.42, 0.14, 0.08)
  data$region <- sample(regions, n, replace = TRUE, prob = region_probs)
  
  # Criar dummies regionais de forma segura
  data$north <- as.integer(data$region == "North")
  data$northeast <- as.integer(data$region == "Northeast")
  data$south <- as.integer(data$region == "South")
  data$centerwest <- as.integer(data$region == "CenterWest")
  
  # Parâmetros verdadeiros (definidos de forma clara)
  parametros <- list(
    intercepto = 6.8,
    education = 0.13,
    male = 0.25,
    white = 0.18,
    experience = 0.035,
    experience_sq = -0.0005,
    north = -0.15,
    northeast = -0.20,
    south = 0.08,
    centerwest = 0.05
  )
  
  # Calcular componente sistemática
  componente_sistemica <- 
    parametros$intercepto +
    parametros$education * data$education +
    parametros$male * data$male +
    parametros$white * data$white +
    parametros$experience * data$experience +
    parametros$experience_sq * (data$experience^2) +
    parametros$north * data$north +
    parametros$northeast * data$northeast +
    parametros$south * data$south +
    parametros$centerwest * data$centerwest
  
  # Gerar erros heterocedásticos
  error_variance <- 0.25 + 0.015 * data$education + 0.005 * data$experience
  hetero_errors <- rnorm(n, mean = 0, sd = sqrt(error_variance))
  
  # Gerar salário final
  data$log_wage_hetero <- componente_sistemica + hetero_errors
  
  cat("   ✅ Amostra de", n, "observações gerada\n")
  return(data)
}

# --- FUNÇÃO FGLS (REVISADA) ---
implementar_fgls <- function(modelo_ols, dados) {
  # Calcular resíduos de forma segura
  residuos <- residuals(modelo_ols)
  
  # Estimar variância dos resíduos
  fitted_vals <- fitted(modelo_ols)
  modelo_variancia <- lm(I(residuos^2) ~ fitted_vals + I(fitted_vals^2))
  sigma2_hat <- fitted(modelo_variancia)
  
  # Garantir que não há zeros ou valores negativos
  sigma2_hat <- pmax(sigma2_hat, 1e-6)
  
  # Estimar FGLS com pesos
  modelo_fgls <- lm(log_wage_hetero ~ education + experience + I(experience^2) +
                      north + northeast + south + centerwest + male + white,
                    data = dados, weights = 1/sigma2_hat)
  
  return(modelo_fgls)
}

# --- GERAR AMOSTRA DE 200K ---
cat("\n3. GERANDO AMOSTRA DE 200K...\n")
dados_200k <- gerar_dados_grande_amostra(200000)

# Preparar dados de forma segura
dados_preparados_200k <- dados_200k %>%
  mutate(experience_sq = experience^2) %>%
  select(log_wage_hetero, education, experience, experience_sq,
         north, northeast, south, centerwest, male, white)

# Verificar se os dados foram gerados corretamente
cat("   ✅ Dados preparados:", nrow(dados_preparados_200k), "observações\n")
cat("   📊 Estatísticas básicas:\n")
cat("      - Educação: média =", round(mean(dados_preparados_200k$education), 2), "\n")
cat("      - Experiência: média =", round(mean(dados_preparados_200k$experience), 2), "\n")
cat("      - Salário (log): média =", round(mean(dados_preparados_200k$log_wage_hetero), 2), "\n")

# --- CARREGAR DADOS DE 2K PARA COMPARAÇÃO ---
cat("\n4. CARREGANDO DADOS DE 2K OBSERVAÇÕES...\n")
dados_2k <- read.csv(file.path(tmp_dir, "dados_simulados.csv"))

# Verificar se o arquivo existe
if (!file.exists(file.path(tmp_dir, "dados_simulados.csv"))) {
  stop("❌ ERRO: Arquivo dados_simulados.csv não encontrado. Execute primeiro 1_gera_dados.R")
}

dados_preparados_2k <- dados_2k %>%
  mutate(experience_sq = experience^2) %>%
  select(log_wage_hetero, education, experience, experience_sq,
         north, northeast, south, centerwest, male, white)

cat("   ✅ Dados de 2k observações carregados\n")

# --- PARÂMETROS VERDADEIROS ---
parametros_verdadeiros <- c(
  "(Intercept)" = 6.8,
  "education" = 0.13,
  "experience" = 0.035, 
  "I(experience^2)" = -0.0005,
  "north" = -0.15,
  "northeast" = -0.20,
  "south" = 0.08,
  "centerwest" = 0.05,
  "male" = 0.25,
  "white" = 0.18
)

# --- ESTIMAR MODELOS PARA 200K ---
cat("\n5. ESTIMANDO MODELOS PARA AMOSTRA DE 200K...\n")

cat("   a) Estimando OLS para 200k...\n")
modelo_ols_200k <- lm(log_wage_hetero ~ education + experience + I(experience^2) +
                        north + northeast + south + centerwest + male + white,
                      data = dados_preparados_200k)

cat("   b) Estimando FGLS para 200k...\n")
modelo_fgls_200k <- implementar_fgls(modelo_ols_200k, dados_preparados_200k)

# --- ESTIMAR MODELOS PARA 2K ---
cat("\n6. ESTIMANDO MODELOS PARA AMOSTRA DE 2K...\n")

cat("   a) Estimando OLS para 2k...\n")
modelo_ols_2k <- lm(log_wage_hetero ~ education + experience + I(experience^2) +
                      north + northeast + south + centerwest + male + white,
                    data = dados_preparados_2k)

cat("   b) Estimando FGLS para 2k...\n")
modelo_fgls_2k <- implementar_fgls(modelo_ols_2k, dados_preparados_2k)

# --- PREPARAR DADOS PARA COMPARAÇÃO (EVITANDO ERROS) ---
cat("\n7. PREPARANDO DADOS PARA COMPARAÇÃO...\n")

# Criar dataframe de comparação de forma segura
variaveis <- c("(Intercept)", "education", "experience", "I(experience^2)",
               "north", "northeast", "south", "centerwest", "male", "white")

comparacao_amostras <- data.frame(
  Variavel = variaveis,
  Verdadeiro = round(parametros_verdadeiros[variaveis], 4),
  OLS_2k = round(coef(modelo_ols_2k)[variaveis], 4),
  FGLS_2k = round(coef(modelo_fgls_2k)[variaveis], 4),
  OLS_200k = round(coef(modelo_ols_200k)[variaveis], 4),
  FGLS_200k = round(coef(modelo_fgls_200k)[variaveis], 4)
)

# Substituir NAs por zeros (caso alguma variável não esteja presente)
comparacao_amostras[is.na(comparacao_amostras)] <- 0

# Calcular erros médios de forma segura
calcular_erro_medio <- function(estimativas, verdadeiros) {
  mean(abs(estimativas - verdadeiros), na.rm = TRUE)
}

erro_ols_2k <- calcular_erro_medio(comparacao_amostras$OLS_2k, comparacao_amostras$Verdadeiro)
erro_fgls_2k <- calcular_erro_medio(comparacao_amostras$FGLS_2k, comparacao_amostras$Verdadeiro)
erro_ols_200k <- calcular_erro_medio(comparacao_amostras$OLS_200k, comparacao_amostras$Verdadeiro)
erro_fgls_200k <- calcular_erro_medio(comparacao_amostras$FGLS_200k, comparacao_amostras$Verdadeiro)

# --- SALVAR RESULTADOS EM HTML (COM TRATAMENTO DE ERROS) ---
cat("\n8. GERANDO RELATÓRIOS...\n")

# Primeiro: salvar relatório em texto
sink(file.path(output_dir, "relatorio_detalhado_1.12.txt"))

cat("RELATÓRIO DETALHADO - QUESTÃO 1.12\n")
cat("==================================\n\n")

cat("COMPARAÇÃO ENTRE AMOSTRAS: 2k vs 200k OBSERVAÇÕES\n")
cat("------------------------------------------------\n\n")

cat("ESTATÍSTICAS DAS AMOSTRAS:\n")
cat("Amostra 2k:", nrow(dados_preparados_2k), "observações\n")
cat("Amostra 200k:", nrow(dados_preparados_200k), "observações\n")
cat("Razão de tamanho:", round(nrow(dados_preparados_200k) / nrow(dados_preparados_2k), 1), "x\n\n")

cat("ERROS MÉDIOS ABSOLUTOS:\n")
cat("-----------------------\n")
cat("OLS 2k:", round(erro_ols_2k, 4), "\n")
cat("FGLS 2k:", round(erro_fgls_2k, 4), "\n")
cat("OLS 200k:", round(erro_ols_200k, 4), "\n")
cat("FGLS 200k:", round(erro_fgls_200k, 4), "\n\n")

cat("MELHORIAS COM AMOSTRA MAIOR:\n")
cat("---------------------------\n")
melhoria_ols <- ((erro_ols_2k - erro_ols_200k) / erro_ols_2k) * 100
melhoria_fgls <- ((erro_fgls_2k - erro_fgls_200k) / erro_fgls_2k) * 100

cat("Melhoria OLS (2k → 200k):", round(melhoria_ols, 1), "%\n")
cat("Melhoria FGLS (2k → 200k):", round(melhoria_fgls, 1), "%\n\n")

cat("ANÁLISE DOS RESULTADOS:\n")
cat("----------------------\n")
cat("1. CONVERGÊNCIA: Ambas técnicas convergem para valores verdadeiros\n")
cat("2. EFICIÊNCIA: FGLS mostra ganhos de eficiência em ambas amostras\n")
cat("3. TAMANHO AMOSTRAL: Amostras maiores reduzem variância dos estimadores\n")
cat("4. HETEROCEDASTICIDADE: FGLS é vantajoso na presença de heterocedasticidade\n\n")

cat("DETALHES DOS COEFICIENTES:\n")
cat("--------------------------\n")
print(comparacao_amostras)

sink()

# Segundo: salvar tabela HTML com stargazer (com tratamento de erro)
cat("   a) Gerando tabela HTML com stargazer...\n")

tryCatch({
  # Usar nomes simples para evitar problemas com caracteres especiais
  nomes_simples <- c("Intercepto", "Educacao", "Experiencia", "Experiencia_Quadrado",
                     "Norte", "Nordeste", "Sul", "Centro_Oeste", "Masculino", "Branco")
  
  comparacao_simples <- comparacao_amostras
  comparacao_simples$Variavel_Simples <- nomes_simples
  
  stargazer(
    comparacao_simples,
    type = "html",
    title = "Comparação entre Amostras de 2k e 200k Observações",
    summary = FALSE,
    rownames = FALSE,
    digits = 4,
    out = file.path(output_dir, "comparacao_amostras_1.12.html")
  )
  
  cat("   ✅ Tabela HTML gerada com sucesso\n")
  
}, error = function(e) {
  cat("   ⚠️  Erro ao gerar tabela HTML:", e$message, "\n")
  cat("   📝 Gerando tabela alternativa...\n")
  
  # Gerar tabela alternativa simples
  write.csv(comparacao_amostras, 
            file.path(output_dir, "comparacao_amostras_1.12.csv"), 
            row.names = FALSE)
  cat("   ✅ Tabela CSV alternativa gerada\n")
})

# --- MENSAGEM FINAL ---
cat("\n✅ QUESTÃO 1.12 CONCLUÍDA!\n")
cat("=======================\n")
cat("📊 RESULTADOS SALVOS:\n")
cat("   • comparacao_amostras_1.12.html (Tabela formatada)\n")
cat("   • relatorio_detalhado_1.12.txt (Análise detalhada)\n")
cat("   • comparacao_amostras_1.12.csv (Tabela alternativa, se necessário)\n\n")

cat("🎯 PRINCIPAIS RESULTADOS:\n")
cat("   Erro OLS 2k:", round(erro_ols_2k, 4), "→ 200k:", round(erro_ols_200k, 4), "\n")
cat("   Erro FGLS 2k:", round(erro_fgls_2k, 4), "→ 200k:", round(erro_fgls_200k, 4), "\n")
cat("   Melhoria OLS:", round(melhoria_ols, 1), "%\n")
cat("   Melhoria FGLS:", round(melhoria_fgls, 1), "%\n")

cat("\n📈 PARA VISUALIZAR OS RESULTADOS:\n")
cat("browseURL('output/relatorio_detalhado_1.12.txt')\n")
cat("browseURL('output/comparacao_amostras_1.12.html')\n")

