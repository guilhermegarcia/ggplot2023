# Curso de Visualização de Dados com ggplot
# Dia 2: Visualizando resultados de modelos

# =================================================

# Carregar pacotes
library(tidyverse)
library(languageR)
library(lme4)
library(sjPlot) #para gerar os gráficos


# Carregar dados
data("english")

# =================================================

# Modelo Linear simples

## Preditor categórico
m1 = lm(data = english, RTlexdec ~ AgeSubject)
summary(m1)

## Preditor contínuo
m2 = lm(data = english, RTlexdec ~ Familiarity)
summary(m2)

# Visualizar tabela em html
tab_model(m1)
tab_model(m2)

# Gráficos 
## dos coeficientes
plot_model(m1)
plot_model(m2)

## dos valores previstos pelo modelo
plot_model(m1, type = "pred")
plot_model(m2, type = "pred") 

## -> O sjPlot chama o ggplot, então podemos adicionar camadas:
plot_model(m2, type = "pred") +
  theme_classic() +
  labs(x = "Familiaridade", y = "Tempo de Reação",
       title = "Valores previstos")


# =================================================

# Modelo Linear múltiplo

m3 = lm(data = english, RTlexdec ~ AgeSubject + Familiarity)
summary(m3)

# Visualizar tabela em html
tab_model(m3)

# Gráficos 
## dos coeficientes
plot_model(m3) # cuidado na comparação direta do tamanho do efeito!

## dos valores previstos pelo modelo
plot_model(m3, type = "pred")

## adicionando as duas variáveis preditoras ao mesmo gráfico
plot_model(m3, type = "pred", terms = c("AgeSubject", "Familiarity"))
plot_model(m3, type = "pred", terms = c("Familiarity", "AgeSubject"))

# Com 3 variáveis preditoras
m4 = lm(data = english, RTlexdec ~ AgeSubject + Familiarity + WordCategory)

# Tabela
tab_model(m4)
tab_model(m3, m4)

# Gráficos
plot_model(m4, type = "pred", terms = c("AgeSubject", "Familiarity",
                                        "WordCategory"))
plot_model(m4, type = "pred", terms = c("WordCategory", "AgeSubject",
                                        "Familiarity"))
plot_model(m4, type = "pred", terms = c("WordCategory", "Familiarity",
                                        "AgeSubject")) +
  theme_light()

## etc...

# Com 4 ou mais variáveis, mais de um gráfico é criado 
m5 = lm(data = english, RTlexdec ~ AgeSubject + Familiarity + WordCategory +
          WrittenFrequency)

plot_model(m5, type = "pred", terms = c("AgeSubject", "Familiarity",
                                        "WordCategory", "WrittenFrequency"))

# =================================================

# Modelo Logístico

m6 = glm(data = english, AgeSubject ~ RTlexdec, family = "binomial")
summary(m6)

# Tabela
tab_model(m6)
tab_model(m6, transform = NULL)

# Gráfico
plot_model(m6, type = "pred")
plot_model(m6, type = "pred", terms="RTlexdec [all]")

# Mais de um preditor
m7 = glm(data = english, AgeSubject ~ RTlexdec + CV, 
         family = "binomial")

plot_model(m7, type = "pred", terms = c("RTlexdec", "CV"))

# =================================================

# Modelo com Interação

m8 = lm(data = english, RTlexdec ~ WrittenFrequency * Familiarity)
summary(m8)

# Tabela
tab_model(m8)

# Gráfico
plot_model(m8, type = "int")
plot_model(m8, type = "pred", terms = c("WrittenFrequency", "Familiarity"))

