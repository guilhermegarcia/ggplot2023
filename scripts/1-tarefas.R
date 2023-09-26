# Curso de Visualização de Dados com ggplot
# Dia 1: Dados descritivos (reposta contínua)

# =================================================
#                     TAREFAS                     
# =================================================

# Carregar os dados "danish" do pacote languageR
library(languageR)

data(danish)

# 1. HISTOGRAMA
# -> Criar um histograma da distribuição de LogRT
# -> utilize os elementos extras e detalhes estéticos que julgar necessários
ggplot('...', aes(x = ..., y = ...)) +
  geom_...

# 2. GRÁFICO DE CAIXAS (ou violino)
# -> Criar um gráfico de caixas com LogRT em função de Sex
# -> utilize os elementos extras e detalhes estéticos que julgar necessários


# 3. GRÁFICO DE DISPERSÃO
# -> Criar um gráfico de caixas com LogRT em função de LogWordFreq
# -> utilize os elementos extras e detalhes estéticos que julgar necessários


# =================================================
#                     CONSULTA                     
# =================================================

# HISTOGRAMA

# Histograma mínimo
ggplot(data = english, aes(x = RTlexdec)) + 
  geom_histogram() 

# Possíveis elementos extras
ggplot(data = english, aes(RTlexdec)) + 
  geom_histogram(binwidth = 0.005, fill = "lightgray", color = "orange") + 
  labs(title = "Histograma", x = "Tempo de reação (log)", y = "") +
  theme_bw() 

# =================================================

# GRÁFICO DE CAIXAS (boxplot)

# Gráfico de caixas mínimo
ggplot(data = english, aes(x = AgeSubject, y = RTlexdec)) +
  geom_boxplot()

# Possíveis elementos extras
ggplot(data = english, aes(x = AgeSubject, y = RTlexdec, color = WordCategory)) +
  geom_boxplot() +
  geom_violin(fill = "violet", alpha = 0.2, color = "white") +
  stat_summary(color = "blue", size = 1, shape = 4) +
  geom_jitter(alpha = 0.05, color = "darkgreen") +
  facet_grid(~WordCategory) +
  labs(title = "Gráfico de caixas",
       x = "Faixa etária",
       y = "Tempo de reação (log)") +
  scale_x_discrete(labels = c("old" = "velhos", "young" = "novos")) +
  theme_light()

# =================================================

# GRÁFICO DE DISPERSÃO (scatterplot)

# Gráfico de dispersão mínimo
ggplot(data = english, aes(x = WrittenFrequency, y = RTlexdec)) + 
  geom_point()

# Possíveis elementos extras
ggplot(data = english, aes(x = WrittenFrequency, y = RTlexdec, color = AgeSubject)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm) +
  labs(title = "Gráfico de dispersão", 
       x = "Frequência escrita",
       y = "Tempo de reação (log)") +
  theme_classic()

# =================================================


