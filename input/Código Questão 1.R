# ================================================================
# EXERCICIO DE ECONOMETRIA: ANALISE DO MERCADO DE TRABALHO BRASILEIRO
# Equacao de Mincer para Retornos à Educacao
# ================================================================

library(dplyr)
library(ggplot2)
library(stargazer)
library(lmtest)
library(sandwich)
library(car)

set.seed(1234) #119063113

# ================================================================
# PARTE 1: GERACAO DOS DADOS
# ================================================================

# Tamanho da amostra
n <- 2000

# Gera variaveis demograficas
data <- data.frame(
  # Educacao: anos de estudo (4 a 18 anos, media = 9.5)
  education = pmax(4, pmin(18, round(rnorm(n, mean = 9.5, sd = 3.5)))),
  
  # Sexo: 1 = masculino, 0 = feminino (52% homens)
  male = rbinom(n, 1, 0.52),
  
  # Raca: 1 = branco, 0 = preto/pardo (45% brancos no Brasil)
  white = rbinom(n, 1, 0.45),
  
  # Experiencia: anos de experiencia no trabalho (0 a 45 anos)
  experience = pmax(0, pmin(45, round(rnorm(n, mean = 15, sd = 10))))
)

# Gera regioes (distribuicao populacional aproximada do Brasil)
regions <- c("North", "Northeast", "Southeast", "South", "CenterWest")
region_probs <- c(0.09, 0.27, 0.42, 0.14, 0.08)
data$region <- sample(regions, n, replace = TRUE, prob = region_probs)

# Cria variaveis dummies para regioes (Sudeste como referencia)
data$north <- ifelse(data$region == "North", 1, 0)
data$northeast <- ifelse(data$region == "Northeast", 1, 0)
data$south <- ifelse(data$region == "South", 1, 0)
data$centerwest <- ifelse(data$region == "CenterWest", 1, 0)

# ================================================================
# PARTE 2: GERACAO DE SALARIOS - ERROS HOMOCEDASTICOS
# ================================================================

# Parametros da equacao de Mincer (baseado na literatura brasileira)
beta_0 <- 6.8        # Intercepto (log do salario base = R$ 900)
beta_edu <- 0.13     # Retorno à educacao (13% por ano de estudo)
beta_male <- 0.25    # Premio masculino (25%)
beta_white <- 0.18   # Premio para brancos (18%)
beta_exp <- 0.035    # Retorno da experiencia (3.5% ao ano)
beta_exp2 <- -0.0005 # Experiencia ao quadrado (retornos decrescentes)

# Efeitos regionais (Sudeste como referencia)
beta_north <- -0.15     # Penalidade Norte
beta_northeast <- -0.20 # Penalidade Nordeste (maior efeito)
beta_south <- 0.08      # Premio Sul
beta_centerwest <- 0.05 # Pequeno premio Centro-Oeste

# Gera salarios logaritmizados com erros HOMOCEDASTICOS
data$log_wage_homo <- beta_0 +
  beta_edu * data$education +
  beta_male * data$male +
  beta_white * data$white +
  beta_exp * data$experience +
  beta_exp2 * data$experience^2 +
  beta_north * data$north +
  beta_northeast * data$northeast +
  beta_south * data$south +
  beta_centerwest * data$centerwest +
  rnorm(n, mean = 0, sd = 0.35) # Erros homocedasticos

# Converte para niveis (salarios em Reais)
data$wage_homo <- exp(data$log_wage_homo)

# ================================================================
# PARTE 3: GERACAO DE SALARIOS - ERROS HETEROCEDASTICOS
# ================================================================

# Variancia dos erros heterocedasticos como funcao de educacao e experiencia
error_variance <- 0.25 + 0.015 * data$education + 0.005 * data$experience

# Gera erros heterocedasticos
hetero_errors <- rnorm(n, mean = 0, sd = sqrt(error_variance))

# Gera salarios logaritmizados com erros HETEROCEDASTICOS
data$log_wage_hetero <- beta_0 +
  beta_edu * data$education +
  beta_male * data$male +
  beta_white * data$white +
  beta_exp * data$experience +
  beta_exp2 * data$experience^2 +
  beta_north * data$north +
  beta_northeast * data$northeast +
  beta_south * data$south +
  beta_centerwest * data$centerwest +
  hetero_errors # Erros heterocedasticos

# Converte para niveis
data$wage_hetero <- exp(data$log_wage_hetero)

# ================================================================
# VERIFICACAO
# ================================================================

# Verifica se funcionou
head(data)
summary(data)