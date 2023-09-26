# Curso de Visualização de Dados com ggplot
# Dia 1: Dados descritivos (reposta contínua)

# =================================================

# Carregar pacotes
library(tidyverse)
library(languageR)

# Carregar dados
data("english")

# =================================================

# HISTOGRAMA
## Para verificar distribuição dos dados

# Histograma mínimo
ggplot(data = english, aes(x = RTlexdec)) + 
  geom_histogram() 

# Adicionando elementos
ggplot(data = english, aes(RTlexdec)) + # pra histograma nem precisaria de x = 
  geom_histogram(binwidth = 0.005, fill = "lightgray", color = "orange") + # quantidade/tamanho de barras, cor, preenchimento
  labs(title = "Histograma", x = "Tempo de reação (log)", y = "") + #legendas
  theme_bw()  # explore outros temas

ggplot(data = english, aes(RTlexdec, fill = AgeSubject, color = AgeSubject)) + # subgrupar por cores
  geom_histogram(binwidth = 0.005) + 
  labs(title = "Histograma", x = "Tempo de reação (log)", y = "") + 
  theme_minimal()  


# =================================================

# GRÁFICO DE CAIXAS (boxplot)
## y contínuo e x categórico

# Gráfico de caixas mínimo
ggplot(data = english, aes(x = AgeSubject, y = RTlexdec)) +
  geom_boxplot()

# Adicionando elementos
ggplot(data = english, aes(x = AgeSubject, y = RTlexdec)) +
  geom_boxplot() +
  stat_summary(color = "blue", size = 1, shape = 4) +
  labs(title = "Gráfico de caixas",
       x = "Faixa etária",
       y = "Tempo de reação (log)") +
  scale_x_discrete(labels = c("old" = "velhos", "young" = "novos")) +
  theme_light()

# Incluindo os dados com geom_jitter
ggplot(data = english, aes(x = AgeSubject, y = RTlexdec)) +
  geom_boxplot() +
  # geom_point() +
  geom_jitter(alpha = 0.05, color = "darkgreen") +
  theme_light()

# subgrupando por cores
ggplot(data = english, aes(x = AgeSubject, y = RTlexdec, color = WordCategory)) +
  geom_boxplot() +
  theme_light()

# subgrupando por paineis
ggplot(data = english, aes(x = AgeSubject, y = RTlexdec)) +
  geom_boxplot() +
  facet_wrap(~WordCategory) +
  theme_light()

ggplot(data = english, aes(x = AgeSubject, y = RTlexdec)) +
  geom_boxplot() +
  facet_grid(CV~WordCategory) +
  theme_light()


# Gráfico de violino
ggplot(data = english, aes(x = AgeSubject, y = RTlexdec)) +
  geom_violin(fill = "violet", alpha = 0.2, color = "white") +
  theme_classic()

# Dá pra sobrepor ao boxplot, e a ordem importa
ggplot(data = english, aes(x = AgeSubject, y = RTlexdec)) +
  geom_boxplot() +
  geom_violin(fill = "violet", alpha = 0.2, color = "white") +
  theme_classic()


# =================================================

# GRÁFICO DE DISPERSÃO (scatterplot)
## y e x contínuos

# Gráfico de dispersão mínimo
ggplot(data = english, aes(x = WrittenFrequency, y = RTlexdec)) + 
  geom_point()

# Adicionando elementos
ggplot(data = english, aes(x = WrittenFrequency, y = RTlexdec)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm) +
  labs(title = "Gráfico de dispersão", 
       x = "Frequência escrita",
       y = "Tempo de reação (log)") +
  theme_classic()

# Sugrupando por cor
ggplot(data = english, aes(x = WrittenFrequency, y = RTlexdec, color = AgeSubject)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm) +
  labs(title = "Gráfico de dispersão", 
       x = "Frequência escrita",
       y = "Tempo de reação (log)") +
  theme_classic()

# Sugrupando por cor apenas as linhas
ggplot(data = english, aes(x = WrittenFrequency, y = RTlexdec)) +  
  geom_point(alpha = 0.1) +
  stat_smooth(method = "lm", aes(color = AgeSubject)) + 
  labs(title = "Gráfico de dispersão", 
       x = "Frequência escrita",
       y = "Tempo de reação (log)") +
  theme_classic()

# Sugrupando por cor apenas os pontos
ggplot(data = english, aes(x = WrittenFrequency, y = RTlexdec)) +  
  geom_point(alpha = 0.3, aes(color = AgeSubject)) +
  stat_smooth(method = "lm") + 
  labs(title = "Gráfico de dispersão", 
       x = "Frequência escrita",
       y = "Tempo de reação (log)") +
  theme_classic()


# =================================================
#                     TAREFAS                     
#           https://bit.ly/workshop-ggplot
# =================================================

# Carregar os dados "danish" do pacote languageR
library(languageR)

data(danish)

# HISTOGRAMA
# -> Criar um histograma da distribuição de LogRT
# -> utilize os elementos extras e detalhes estéticos que julgar necessários
ggplot(danish, aes(LogRT)) +
  geom_histogram()

# GRÁFICO DE CAIXAS (ou violino)
# -> Criar um gráfico de caixas com LogRT em função de Sex
# -> utilize os elementos extras e detalhes estéticos que julgar necessários
ggplot(danish, aes(y = LogRT, x = Sex)) +
  geom_boxplot()

# GRÁFICO DE DISPERSÃO
# -> Criar um gráfico de caixas com LogRT em função de LogWordFreq
# -> utilize os elementos extras e detalhes estéticos que julgar necessários
ggplot(danish, aes(y = LogRT, x = LogWordFreq)) +
  geom_point() +
  geom_smooth(method = lm)

