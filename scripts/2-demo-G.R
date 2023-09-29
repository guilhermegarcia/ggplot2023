library(tidyverse)
library(languageR)
library(scales)

d = english |> 
  as_tibble()

glimpse(d)

# Criar figura simples:
ggplot(data = d, aes(x = Familiarity, y = RTlexdec)) +
  geom_point(alpha = 0.3) +
  theme_minimal(base_size = 12)

# Salvar/exportar figura (jpeg)
# ggsave(last_plot(),
#        filename = "scripts/Figuras/meuGrafico_dia2.jpeg",
#        width = 5,
#        height = 5,
#        dpi = 800)

# ====================== TRANSFORMACAO DE TABELA

original = read_csv("dados/exemplo.csv") # gdgarcia.ca/ggplot

original

# LONGO
# participant     group     teste     nota
# subject_1       control   A          4.4
# subject_1       control   B          6.9
# subject_1       control   C          6.3

long = original |> 
  pivot_longer(names_to = "teste",
               values_to = "nota",
               cols = testA:testC)


original |> 
  pivot_longer(names_to = "teste",
               values_to = "nota",
               cols = testA:testC) |> 
  ggplot(data = _, aes(x = teste, y = nota)) +
  stat_summary() + 
  theme_classic()


long # TIDY DATA: 1 variável por coluna; 1 observação por linha


ggplot(data = long, aes(x = teste, y = nota)) + 
  stat_summary()


wide = long |> 
  pivot_wider(names_from = "teste",
              values_from = "nota") |> 
  mutate(CmenosB = testC - testB) |> 
  pivot_longer(names_to = "teste",
               values_to = "nota",
               cols = testA:testC)

wide

# ================================= DADOS CATEGORICOS

d = english |> 
  as_tibble() |> 
  select(AgeSubject, RTlexdec)


d
# tempo de reação ~ idade (grupo etário)
ggplot(data = d, aes(x = AgeSubject, y = RTlexdec)) + 
  geom_boxplot()

# P(AgeSubject = Old) ~ tempo de reação
ggplot(data = d, aes(x = AgeSubject, y = RTlexdec)) + 
  geom_boxplot() +
  coord_flip()

ggplot(data = d |> mutate(Old = if_else(AgeSubject == "old", 1, 0)),
       aes(x = RTlexdec |> exp(), y = Old)) +
  geom_jitter(alpha = 0.1, height = 0.1) +
  labs(x = "Tempo de reação em ms") +
  # geom_smooth(method = "lm", color = "red") +
  geom_smooth(method = "glm", color = "blue", method.args = list(family = "binomial")) +
  coord_cartesian(ylim = c(0, 1))


# ==========  EXEMPLO 2
hvd = read_csv("dados/hvd.csv")

# Verificar classe das variáveis:

hvd = hvd |> 
  mutate(across(where(is_character), as_factor))

hvd

hvd |> 
  filter(type != "filler") |> 
  group_by(numSyll, responseFoot) |> 
  count() |> 
  # ungroup() |> 
  group_by(responseFoot) |> 
  mutate(prop = n / sum(n)) |> 
  mutate(numSyll = as_factor(numSyll)) |>   
  ggplot(data = _, aes(x = responseFoot, y = prop, fill = numSyll)) + 
  geom_col(position = position_dodge())

# Importe os dados "pratica.csv" e crie um gráfico com proporções para a coluna response
# Use barras com cores que representam a língua materna dos participantes.
# Faça ajustes estéticos de sua preferência. Nome para o grafico: "dia2-pratica1-nome.jpeg"
# Ponha o gráfico aqui: https://bit.ly/seugraficoaqui

# Importar dados:
pratica = read_csv("dados/pratica.csv")

# Explorar variáveis:
glimpse(pratica)

# Transformar em proporções:
props = pratica |> 
  group_by(L1, response) |> 
  count() |> 
  group_by(L1) |> 
  mutate(prop = n / sum(n))

props

# Criar figura:
ggplot(data = props, aes(x = L1, y = prop, fill = response)) + 
  geom_col(position = position_dodge(), color = "black", alpha = 0.9) + 
  theme_classic(base_size = 15,
                base_family = "Comic Sans MS") + 
  labs(x = "Língua materna",
       y = NULL,
       fill = "Resposta:",
       title = "Nosso gráfico para respostas high e low",
       subtitle = "Nunca use Comic Sans em um gráfico!") +
  scale_x_discrete(labels = c("English" = "Inglês",
                              "Spanish" = "Espanhol")) + 
  scale_y_continuous(labels = percent_format()) +
  coord_cartesian(ylim = c(0, 1)) +
  # scale_fill_brewer(palette = 7)
  scale_fill_manual(values = c("darkorange2", "steelblue3")) +
  theme(legend.position = "bottom")


# Usar cor em título (de forma funcional):

cores = c("darkorange2", "steelblue3")
names(cores) = c("high", "low")

# Criar título:

titulo = glue::glue('Nosso gráfico para respostas <span style="color:{cores["high"]}">**high**</span> e 
<span style="color:{cores["low"]}">**low**</span>')


ggplot(data = props, aes(x = L1, y = prop, fill = response)) + 
  geom_col(position = position_dodge(), color = "black", alpha = 0.9) + 
  theme_classic(base_size = 15,
                base_family = "Comic Sans MS") + 
  labs(x = "Língua materna",
       y = NULL,
       fill = "Resposta:",
       title = titulo,
       subtitle = "Nunca use Comic Sans em um gráfico!") +
  scale_x_discrete(labels = c("English" = "Inglês",
                              "Spanish" = "Espanhol")) + 
  scale_y_continuous(labels = percent_format()) +
  coord_cartesian(ylim = c(0, 1)) +
  # scale_fill_brewer(palette = 7)
  scale_fill_manual(values = c("darkorange2", "steelblue3")) +
  theme(legend.position = "none",
        plot.title = ggtext::element_markdown(hjust = 0.5))
