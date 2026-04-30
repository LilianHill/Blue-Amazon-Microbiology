#Pacotes
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(tm)
library(stringr)
library(openxlsx)
library(iNEXT)
library(ggthemes)
library(reshape2)

# Dados #substituir pelos seus dados

# Ref
file_path <- 'C:/Users/lilia/OneDrive/Documentos/LECOM/INCT-BAA/ref_final.xlsx'
ref <- read_excel(file_path)

# Spp
file_path <- 'C:/Users/lilia/OneDrive/Documentos/LECOM/INCT-BAA/spp_ecorr_ok.xlsx'
spp <- read_excel(file_path)

#### 5. ACUMULADO POR REGIÃO #####
# Separar as regiões #
#### 4. CRESCIMENTO POR REGIÃO ####

library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

publicacoes_por_regiao <- ref %>%
  separate_rows(Region, sep = " \\| ") %>%
  mutate(
    Year = as.numeric(Year)  # Converter Year para numérico
  ) %>%
  filter(
    Region %in% c("NE", "SE", "S", "N"),
    !is.na(Year) & Year <= 2023
  ) %>%
  group_by(Year, Region) %>%
  summarise(quantidade = n(), .groups = 'drop') %>%
  arrange(Year)

publicacoes_por_regiao <- publicacoes_por_regiao %>%
  group_by(Region) %>%
  mutate(acumulado = cumsum(quantidade)) %>%
  mutate(media_movel = zoo::rollmean(acumulado, 3, fill = NA, align = "center")) %>%
  ungroup()

total_publicacoes_por_regiao <- publicacoes_por_regiao %>%
  group_by(Region) %>%
  summarise(total_publicacoes = max(acumulado), .groups = 'drop')

# Gráfico

ggplot(publicacoes_por_regiao, aes(x = Year, y = acumulado, color = Region)) +
  geom_line(size = 0.8, alpha = 0.3) +
  geom_point(data = subset(publicacoes_por_regiao, acumulado > 0), size = 1.5, alpha = 0.3) +
  geom_line(aes(y = media_movel), size = 1, linetype = "solid") +
  labs(title = "", x = "Year", y = "Cumulative Number of Publications", color = "Region") +
  scale_color_manual(
    values = c("NE" = "#F1A7C1", "SE" = "#A2C8D9", "S" = "#9FDAAD", "N" = "#F4D66A"),
    labels = function(x) paste0(x, " (", total_publicacoes_por_regiao$total_publicacoes[match(x, total_publicacoes_por_regiao$Region)], ")")
  ) +
  scale_x_continuous(
    limits = c(1975, 2025),
    breaks = seq(1975, 2024, by = 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, max(publicacoes_por_regiao$acumulado, na.rm = TRUE), by = 50),
    expand = c(0, 0)
  ) +
  theme(
    panel.background = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 9),
    legend.position = c(0.08, 0.9),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

#### 6. ACUMULADO POR ECOREGIÃO (usando ECOREGION de spp) ####

library(dplyr)
library(tidyr)
library(ggplot2)

# Passo 1: Preparar a tabela spp com ECOREGION
# Selecionar apenas Number e ECOREGION de spp
spp_ecoregion <- spp %>%
  select(Number, ECOREGION) %>%
  filter(!is.na(ECOREGION) & ECOREGION != "NA" & ECOREGION != "") %>%
  # Converter Number para character para garantir compatibilidade
  mutate(Number = as.character(Number))

# Se houver múltiplas linhas com mesmo Number mas diferentes ECOREGION em spp,
# manter apenas a mais frequente (ou a primeira)
spp_ecoregion <- spp_ecoregion %>%
  group_by(Number, ECOREGION) %>%
  summarise(n = n(), .groups = 'drop') %>%
  arrange(Number, desc(n)) %>%
  group_by(Number) %>%
  slice(1) %>%
  ungroup() %>%
  select(Number, ECOREGION)

# Passo 2: Juntar ref com spp_ecoregion para adicionar ECOREGION em ref
ref_com_ecoregion <- ref %>%
  mutate(Number = as.character(Number)) %>%
  left_join(spp_ecoregion, by = "Number") %>%
  filter(!is.na(ECOREGION) & ECOREGION != "NA" & ECOREGION != "")

# Verificar quantas publicações ficaram com ECOREGION
cat("\n=== DIAGNÓSTICO - ACUMULADO POR ECOREGIÃO ===\n")
cat("Total de publicações em ref:", nrow(ref), "\n")
cat("Publicações com ECOREGION (após join com spp):", nrow(ref_com_ecoregion), "\n")
cat("Publicações sem ECOREGION:", nrow(ref) - nrow(ref_com_ecoregion), "\n\n")

# Passo 3: Separar ecorregiões combinadas em linhas distintas
ref_expandido <- ref_com_ecoregion %>%
  separate_rows(ECOREGION, sep = " *, *") %>%
  separate_rows(ECOREGION, sep = " \\| ") %>%
  mutate(
    ECOREGION = str_trim(ECOREGION),
    Year = as.numeric(Year)
  ) %>%
  filter(!is.na(ECOREGION) & ECOREGION != "NA" & ECOREGION != "" & 
           !is.na(Year) & Year <= 2023)

# Passo 4: Contar publicações únicas por ano e ecorregião
# IMPORTANTE: distinct(Number, Year, ECOREGION) garante que cada publicação seja contada apenas uma vez
publicacoes_por_ecorregiao <- ref_expandido %>%
  distinct(Number, Year, ECOREGION) %>%
  count(ECOREGION, Year, name = "n") %>%
  arrange(ECOREGION, Year) %>%
  group_by(ECOREGION) %>%
  mutate(acumulado = cumsum(n)) %>%
  ungroup()

# Mostrar resumo das ecorregiões
cat("Total de publicações acumuladas por ecorregião:\n")
total_por_ecoregiao <- publicacoes_por_ecorregiao %>%
  group_by(ECOREGION) %>%
  summarise(total = max(acumulado)) %>%
  arrange(desc(total))
print(total_por_ecoregiao)

# Gráfico
ggplot(publicacoes_por_ecorregiao, aes(x = Year, y = acumulado, color = ECOREGION)) +
  geom_line(size = 0.8, alpha = 0.08) +
  geom_point(data = subset(publicacoes_por_ecorregiao, acumulado > 0), size = 1.5, alpha = 0.08) +
  geom_line(size = 1, linetype = "solid") +
  labs(
    title = "",
    x = "Year",
    y = "Cumulative Number of Publications",
    color = "ECOREGION"
  ) +
  scale_color_discrete() +
  scale_x_continuous(
    limits = c(1975, 2025),
    breaks = seq(1975, 2024, by = 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, max(publicacoes_por_ecorregiao$acumulado, na.rm = TRUE), by = 50),
    expand = c(0, 0)
  ) +
  theme(
    panel.background = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 12),
    legend.position = c(0.2, 0.7),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )