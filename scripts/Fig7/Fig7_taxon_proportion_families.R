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
  

#### Carregar Dados ####

# spp <-

spp_limpo <- spp %>%
  filter(!is.na(Family), Family != "NA",
         !is.na(Date_year), Date_year != "NA",
         !is.na(Habitat), Habitat != "NA") %>%
  mutate(
    Family = str_squish(Family),
    Date_year = suppressWarnings(as.integer(Date_year)),
    Habitat = str_squish(Habitat)
  ) %>%
  filter(!is.na(Date_year))

#### Top 50 Famílias mais frequentes ####

top_families <- spp_limpo %>%
  filter(!is.na(Family), Family != "NA") %>%
  count(Family, sort = TRUE) %>%
  slice_max(n, n = 50) %>%
  pull(Family)

spp_top <- spp_limpo %>%
  filter(Family %in% top_families) %>%
  filter(!is.na(Date_year)) %>%
  mutate(Date_year = suppressWarnings(as.integer(Date_year))) %>%
  filter(!is.na(Date_year))

min_year <- floor(min(spp_top$Date_year, na.rm = TRUE) / 5) * 5
max_year <- ceiling(max(spp_top$Date_year, na.rm = TRUE) / 5) * 5

spp_top <- spp_top %>%
  mutate(quinquenio = cut(Date_year,
                          breaks = seq(min_year, max_year, by = 5),
                          right = FALSE)) %>%
  filter(!is.na(quinquenio)) %>%
  count(Family, quinquenio)

spp_top <- spp_top %>%
  group_by(Family) %>%
  mutate(total_n = sum(n)) %>%
  ungroup() %>%
  mutate(Family = fct_reorder(Family, total_n, .desc = TRUE))

ggplot(spp_top, aes(x = quinquenio, y = Family, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Records", option = "C") +
  labs(title = "",
       x = "5-year period", y = "Family") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(face = "bold"))+
  scale_y_discrete(limits = rev)

ggplot(spp_top, aes(x = quinquenio, y = Family, size = n)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  scale_size_continuous(name = "Records") +
  labs(title = "Temporal distribution of top 50 families (5-year intervals)",
       x = "5-year period", y = "Family") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(face = "bold"))
