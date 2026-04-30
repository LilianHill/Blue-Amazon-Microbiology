# Pacotes
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

# Dados - substituir pelos seus caminhos

# Ref
file_path <- 'C:/Users/lilia/OneDrive/Documentos/LECOM/INCT-BAA/ref_final.xlsx'
ref <- read_excel(file_path)

# Spp
file_path <- 'C:/Users/lilia/OneDrive/Documentos/LECOM/INCT-BAA/spp_ecorr_ok.xlsx'
spp <- read_excel(file_path)

#### 4. CRESCIMENTO POR ECORREGIÃO (usando ECOREGION de spp) ####

# Passo 1: Preparar a tabela spp com ECOREGION
# Selecionar apenas Number e ECOREGION de spp, e remover duplicatas
# (cada Number deve ter apenas uma ECOREGION)
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
cat("Total de publicações em ref:", nrow(ref), "\n")
cat("Publicações com ECOREGION (após join com spp):", nrow(ref_com_ecoregion), "\n")
cat("Publicações sem ECOREGION:", nrow(ref) - nrow(ref_com_ecoregion), "\n\n")

# Passo 3: Separar ecorregiões combinadas em linhas distintas
# (caso uma publicação tenha múltiplas ecorregiões separadas por vírgula ou |)
ref_expandido <- ref_com_ecoregion %>%
  separate_rows(ECOREGION, sep = " *, *") %>%
  separate_rows(ECOREGION, sep = " \\| ") %>%
  mutate(ECOREGION = str_trim(ECOREGION)) %>%
  filter(!is.na(ECOREGION) & ECOREGION != "NA" & ECOREGION != "")

# Passo 4: Contar publicações únicas por ano e ecorregião
# IMPORTANTE: Usar distinct(Number, Year, ECOREGION) para não contar a mesma publicação duas vezes
publicacoes_por_ecoregion <- ref_expandido %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Year) & Year <= 2023) %>%
  distinct(Number, Year, ECOREGION) %>%  # Garantir que cada Number é contado apenas uma vez por ano/ecorregião
  group_by(Year, ECOREGION) %>%
  summarise(quantidade = n(), .groups = 'drop') %>%
  arrange(Year)

# Mostrar resumo das ecorregiões encontradas
cat("Ecorregiões encontradas e número de publicações:\n")
print(publicacoes_por_ecoregion %>%
        group_by(ECOREGION) %>%
        summarise(total_publicacoes = sum(quantidade)) %>%
        arrange(desc(total_publicacoes)))

# Passo 5: Calcular a média móvel
publicacoes_por_ecoregion <- publicacoes_por_ecoregion %>%
  group_by(ECOREGION) %>%
  mutate(media_movel = zoo::rollmean(quantidade, 3, fill = NA, align = "center")) %>%
  ungroup()

# Passo 6: Gerar o Gráfico
ggplot(publicacoes_por_ecoregion, aes(x = Year, y = quantidade, color = ECOREGION)) +
  geom_line(size = 0.8, alpha = 0.08) +
  geom_point(data = subset(publicacoes_por_ecoregion, quantidade > 0), size = 1.5, alpha = 0.08) +
  geom_line(aes(y = media_movel), size = 1, linetype = "solid") +
  labs(title = "", x = "Year", y = "Number of Publications", color = "ECOREGION") +
  scale_color_discrete() +
  scale_x_continuous(
    limits = c(1975, 2025),
    breaks = seq(1975, 2024, by = 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, max(publicacoes_por_ecoregion$quantidade, na.rm = TRUE), by = 5),
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