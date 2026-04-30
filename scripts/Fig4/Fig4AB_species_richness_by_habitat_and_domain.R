# criado por: Marília Previero
# modificado em: 22/05/25
#-------------------------------------------------------------------------------
# Análises sobre os habitats e sub-habitats estudados ##########################


library(tidyverse)
library(stringr)
library(patchwork)
library(ggsci)      # para paleta de cores 
library(packcircles)
library(viridis)

# Definindo o diretório 
dsn <- "/home/marilia/Documentos/pesquisas/INCT/Parte 2 Análises Cefalópodes/scripts/arquivos"
setwd(dsn)

# Importando a planilha referências
plan_ref <- read.csv("planilha referencias completa.csv") |> as_tibble()

# ============================
# HABITAT por REGIÃO
# ============================
# processo de separação dos habitats e regiões pelo delimitador |
plan_habitat <- plan_ref %>% 
  select(Number, Year, Habitat, Region) %>%
  mutate(Habitat = str_split(Habitat, " \\| "),
         Region  = str_split(Region,  " \\| ")) %>%
  unnest(Habitat) %>% mutate(Habitat = str_squish(Habitat)) %>%
  unnest(Region)  %>% mutate(Region  = str_squish(Region)) %>%
  na.omit()

n_trab_reg <- plan_habitat %>%
  group_by(Habitat, Region) %>%
  summarise(n = n_distinct(Number), .groups = "drop")

# ============================
# SUB-HABITATS
# ============================
# processo de separação dos habitats, sub-habitats e regiões pelo delimitador |
plan_sub_habitat <- plan_ref %>%
  select(Number, Year, Habitat, Habitat_sub, Region) %>%
  mutate(Habitat     = str_split(Habitat,     " \\| "),
         Habitat_sub = str_split(Habitat_sub, " \\| "),
         Region      = str_split(Region,      " \\| ")) %>%
  unnest(Habitat)     %>% mutate(Habitat     = str_squish(Habitat)) %>%
  unnest(Habitat_sub) %>% mutate(Habitat_sub = str_squish(Habitat_sub)) %>%
  unnest(Region)      %>% mutate(Region      = str_squish(Region))

# Correspondência entre Habitat e Sub-habitat
# adaptar aos seus dados, precisa repetir o habitat para cada respectivo sub-habitat
correspondencias_habitat_e_habitat_sub <- data.frame(
  Habitat = c("Continental shelf", "Continental shelf", "Continental shelf", "Continental shelf",
              "Continental shelf", "Continental shelf", "Continental shelf", "Continental shelf",
              "Continental shelf", "Continental shelf", "Continental shelf", "Continental shelf",
              "Estuary", "Estuary", "Oceanic island", "Oceanic island",
              "Sandy beach", "Sandy beach"),
  Habitat_sub = c("Bay", "Intertidal area", "Subtidal area", "Basin", "Bight", "Channel",
                  "Cove", "Gulf", "Peninsula", "Artificial structure", "Continental island",
                  "Submarine canyons", "Intertidal area", "Subtidal area", "Archipelago",
                  "Atoll", "Intertidal area", "Subtidal area")
)

# ============================================
# LIMPEZA E CONTAGEM DE SUB-HABITATS VÁLIDOS
# ============================================
# limpar e preparar os dados de Sub-habitats válidos associados aos Habitats principais
plan_sub_habitat_clean <- plan_sub_habitat %>%
  left_join(correspondencias_habitat_e_habitat_sub %>% 
  mutate(valido = TRUE), by = c("Habitat", "Habitat_sub")) %>%
  mutate(Habitat_sub = ifelse(is.na(valido), NA, Habitat_sub)) %>% 
  filter(Habitat %in% c("Continental shelf", "Estuary", "Oceanic island", "Sandy beach")) %>%
  group_by(Habitat, Habitat_sub) %>%
  summarise(n = n_distinct(Number), .groups = "drop")

# Adicionando proporções e posição dos rótulos
plan_sub_habitat_5_donut <- plan_sub_habitat_clean %>%
  group_by(Habitat) %>%
  mutate(
    total     = sum(n),
    pct       = n / total,
    label_pos = cumsum(n) - n / 2
  )

plan_sub_habitat_5_donut <- plan_sub_habitat_clean %>%
  group_by(Habitat) %>%
  mutate(
    total     = sum(n),
    pct       = n / total,
    label_pos = cumsum(n) - n / 2
  )

# ============================
# GRÁFICOS INDIVIDUAIS
# ============================
habitat_regiao <- ggplot(n_trab_reg, aes(y = reorder(Habitat, n), x = n, fill = Region)) +
  geom_bar(stat = "identity") +
  scale_fill_npg() +
  labs(
    title = "Estudos por Habitat e Região",
    x     = "Number of studies",
    y     = "Habitat" ) +
  theme_linedraw(base_size = 13) +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5),
    plot.title   = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.box   = "vertical",
    panel.grid   = element_blank())

habitat_regiao

# Gráfico de Rosquinha de Sub-habitat
sub_habitat <- ggplot(plan_sub_habitat_5_donut, aes(x = 2, y = n, fill = Habitat_sub)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 0.1) +
  coord_polar("y") +
  facet_wrap(~Habitat, scales = "free") +
  xlim(c(1, 2.5)) +
  ggtitle("Sub-habitats estudados em cada Habitat") +
  theme_classic(base_size = 13) +
  theme(
    axis.title    = element_blank(),
    axis.text     = element_blank(),
    axis.ticks    = element_blank(),
    panel.grid    = element_blank(),
    strip.text    = element_text(size = 11, face = "bold"),
    legend.title  = element_blank(),
    legend.text   = element_text(size = 11),
    plot.title    = element_text(size = 15, face = "bold", hjust = 0.1))

sub_habitat

# ============================
# COMBINAR COM PATCHWORK
# ============================
final_plot <- habitat_regiao + sub_habitat+
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    theme = theme(
      plot.title   = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0.5)))

final_plot

# ================================================
# OUTROS TIPOS DE GRÁFICO PARA HABITAT POR REGIÃO
# ================================================
n_trab_heat <- plan_habitat %>%
  group_by(Region, Habitat) %>%
  summarise(n = n_distinct(Number), .groups = "drop")

# ============
# HEATMAP
# ============
heatmap_plot <- ggplot(n_trab_heat, aes(y = reorder(Habitat, n), x = Region, fill = n)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis(name = "Nº of studies", option = "D", direction = -1) +
  labs(
    title = "Estudos por Habitat e Região",
    x     = "Region",
    y     = "Habitat") +
  theme_linedraw(base_size = 13) +
  theme(
    axis.text.x        = element_text(angle = 0, hjust = 0.5),
    plot.title         = element_text(size = 12, face = "bold", hjust = 0.5),
    panel.grid.major.x = element_line())

 heatmap_plot

# =============
# BALLOON PLOT
# =============
balloon_plot <- ggplot(n_trab_heat, aes(x = Region, y = reorder(Habitat, n), size = n, fill = n)) +
  geom_point(shape = 21, color = "black", stroke = 0.3) +
  scale_size(range = c(2, 14), name = "Nº of studies") +
  scale_fill_viridis_c(option = "D", direction = -1, name = "Nº of studies") +
  labs(
    title = "Estudos por Habitat e Região",
    x     = "Region",
    y     = "Habitat") +
  theme_linedraw(base_size = 13) +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5),
    plot.title   = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.box   = "vertical",
    panel.grid   = element_blank())

balloon_plot

# ========================================
# COMBINAR GRÁFICOS COM o pacote PATCHWORK
# ========================================

heatmap_plot + balloon_plot + habitat_regiao

