#### Carregando Pacotes ####

library (readxl)
library(ggplot2)
library (dplyr)
library(tidyr)
library(zoo)
library(tm)
library(wordcloud)
library(ggalluvial)
library(stringr)
library(openxlsx)
library(patchwork)
library(iNEXT)
library(ggthemes)
library (vegan)
library(reshape2)
library(betapart)
library(writexl)
library(viridis)
library(forcats)


#### Carregar Dados ####

#spp <-
  
#### Quantitativo de spp por década em relação ao esforço amostral ####  
spp_limpo <- spp %>%
  # Remover NAs ou "NA" em colunas chave
  filter(!is.na(Species), Species != "NA",
         !is.na(Date_year), Date_year != "NA",
         !is.na(Habitat), Habitat != "NA") %>%
  
  # Limpar e padronizar texto
  mutate(
    Species = str_squish(Species),
    Date_year = suppressWarnings(as.integer(Date_year)),
    Habitat = str_squish(Habitat)
  ) %>%
  
  # Remover espécies genéricas (com " sp." fora de parênteses)
  filter(!str_detect(Species, "\\bsp\\.(?!\\))")) %>%
  
  # Manter apenas linhas com ano legível
  filter(!is.na(Date_year))


# Espécies únicas por década (primeiro registro)
riqueza_decada_unica <- spp_limpo %>%
  filter(!is.na(Date_year), !is.na(Species),
         Date_year != "NA", Species != "NA") %>%
  separate_rows(Date_year, sep = " \\| ") %>%
  mutate(Date_year = as.integer(Date_year)) %>%
  filter(Date_year >= 1900, Date_year <= 2100) %>%
  group_by(Species) %>%
  summarise(first_year = min(Date_year), .groups = "drop") %>%
  mutate(decade = floor(first_year / 10) * 10) %>%
  count(decade, name = "unique_species")

# Número de trabalhos por década
trabalhos_decada <- ref %>%
  filter(!is.na(Year)) %>%
  mutate(Year = as.integer(Year),
         decade = floor(Year / 10) * 10) %>%
  count(decade, name = "num_trabalhos")

# Juntando os dois
dados_combinados <- riqueza_decada_unica %>%
  full_join(trabalhos_decada, by = "decade") %>%
  replace_na(list(unique_species = 0, num_trabalhos = 0)) %>%
  arrange(decade)

# Gráfico
ggplot(dados_combinados, aes(x = factor(decade))) +
  geom_col(aes(y = unique_species), fill = "steelblue") +
  geom_line(aes(y = num_trabalhos, group = 1), color = "black", size = 1.2) +
  geom_point(aes(y = num_trabalhos), color = "black", size = 2) +
  labs(title = "",
       x = "Decade",
       y = "Number of species (bar) / studies (line)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(face = "bold")) + scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
