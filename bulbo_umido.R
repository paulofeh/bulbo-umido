# Carregar pacotes necessários
library(tidyverse)
library(ggalt)

# Carregar os dados
temp_1981_2010 <- read_csv("C:/Users/paulo/OneDrive - Insper - Institudo de Ensino e Pesquisa/15 Datavis Studio II/temperaturas_1981_2010.csv")
temp_1991_2020 <- read_csv("C:/Users/paulo/OneDrive - Insper - Institudo de Ensino e Pesquisa/15 Datavis Studio II/temperaturas_1991_2020.csv")

# Criar as colunas de temperatura máxima por estação
tmax_1981_2010 <- temp_1981_2010 %>%
  rowwise() %>%
  mutate(tmax = max(c_across(Janeiro:Dezembro), na.rm = TRUE)) %>%
  ungroup() %>%
  select(Código, `Nome da Estação`, UF, tmax)

tmax_1991_2020 <- temp_1991_2020 %>%
  rowwise() %>%
  mutate(tmax = max(c_across(Janeiro:Dezembro), na.rm = TRUE)) %>%
  ungroup() %>%
  select(Código, `Nome da Estação`, UF, tmax)

# Mesclar as duas bases e renomear colunas
tmax_comparacao <- inner_join(tmax_1981_2010, tmax_1991_2020, by = c("Código", "Nome da Estação", "UF")) %>%
  rename(tmax2010 = tmax.x, tmax2020 = tmax.y)

# Filtrar as estações com temperatura máxima acima de 25°C em qualquer dos períodos
tmax_filtrado <- tmax_comparacao %>%
  filter(tmax2010 > 25 | tmax2020 > 25)

# Calcular a variação de temperatura
tmax_filtrado <- tmax_filtrado %>%
  mutate(variacao = case_when(
    tmax2020 > tmax2010 ~ "aumento",
    tmax2020 < tmax2010 ~ "redução",
    TRUE ~ "estabilidade"
  ))

# Ordenar pelo valor de tmax2020 em ordem decrescente
tmax_filtrado2 <- tmax_filtrado %>%
  arrange(desc(tmax2020)) %>%
  mutate(Estacao_UF = factor(paste(`Nome da Estação`, "(", UF, ")", sep = ""), levels = paste(`Nome da Estação`, "(", UF, ")", sep = "")))

# Criar o gráfico
ggplot(tmax_filtrado2, aes(x = tmax2010, xend = tmax2020, y = Estacao_UF, group = Estacao_UF)) +
  geom_dumbbell(aes(colour = variacao), size = 1.5, 
                colour_x = "#ffa600", colour_xend = "#003f5c") +
  geom_point(aes(x = tmax2010, y = Estacao_UF, color = "1981-2010"), size = 3) +
  geom_point(aes(x = tmax2020, y = Estacao_UF, color = "1991-2020"), size = 3) +
  scale_color_manual(name = "Legenda",
                     values = c("1981-2010" = "#ffa600", "1991-2020" = "#003f5c", 
                                "aumento" = "#e67f83", "redução" = "#6aaa96", "estabilidade" = "gray"),
                     breaks = c("1981-2010", "1991-2020", "aumento", "redução"),
                     labels = c("1981-2010", "1991-2020", "Aumento", "Redução")) +
  labs(x = "Temperatura máxima (°C)", y = "Estação", 
       title = "Variação da temperatura de bulbo úmido entre 1981-2010 e 1991-2020",
       subtitle = "Temperaturas máximas por estação (tmax > 25°C)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9, face = "plain"),
        axis.title.x = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "right",
        legend.box = "vertical") +
  guides(colour = guide_legend(override.aes = list(size = 3)))

