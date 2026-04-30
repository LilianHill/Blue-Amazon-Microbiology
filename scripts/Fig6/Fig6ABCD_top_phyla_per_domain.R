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

##### Top Filos ####
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


# Limpeza e contagem
top_phyla <- spp_limpo %>%
  filter(!is.na(Phylum), Phylum != "NA") %>%
  distinct(Species, Phylum) %>%
  count(Phylum, sort = TRUE)

# Separa top 10 e agrupa o restante como "Others"
top10 <- top_phyla %>% slice(1:10)
others <- top_phyla %>% slice(11:n()) %>%
  summarise(n = sum(n)) %>%
  mutate(Phylum = "Others")

# Junta dados
top_phyla_plot <- bind_rows(top10, others)

# Define ordem com "Others" por último
top_phyla_plot$Phylum <- factor(top_phyla_plot$Phylum,
                                levels = c(top10$Phylum, "Others"))

# Gráfico final
ggplot(top_phyla_plot, aes(x = reorder(Phylum, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(x = "Phylum", y = "Number of species",
       title = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  # eixo X começa exatamente no 0
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(face = "bold")
  )
