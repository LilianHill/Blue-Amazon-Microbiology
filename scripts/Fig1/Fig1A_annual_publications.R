# Pacotes
library (readxl)
library(ggplot2)
library (dplyr)
library(tidyr)
library(zoo)
install.packages("tm")
library(tm)
library(stringr)
library(openxlsx)
library(iNEXT)
library(ggthemes)
library(reshape2)

# Dados  #substituir pelos seus dados

# Ref
file_path <- 'C:/Users/lilia/OneDrive/Documentos/LECOM/INCT-BAA/ref_INCT_ecor.xlsx'
ref <- read_excel(file_path)

# Spp
file_path <- 'C:/Users/lilia/OneDrive/Documentos/LECOM/INCT-BAA/spp_INCT_ecor.xlsx'
spp <- read_excel(file_path)

#### 1. PLOT DE PUBLICAÇÕES POR ANO ######
# Contar o número de publicações por ano
publicacoes_por_ano <- ref %>%
  group_by(Year) %>%
  summarise(quantidade = n()) %>%
  arrange(Year) %>%
  filter(Year != 2024)  # Filtro para ano incompleto 

# Cálculo da média móvel
publicacoes_por_ano$media_movel <- zoo::rollmean(publicacoes_por_ano$quantidade, 3, fill = NA)

# Plot
ggplot(publicacoes_por_ano, aes(x = Year, y = quantidade)) +
  geom_line(color = "black", linewidth = 0.5) +  
  geom_point(color = "black", size = 0.5) + # Pontos nos anos
  geom_line(aes(y = media_movel), color = "grey", linewidth = 0.4) +  # Média móvel (padrão mais suage dos dados)
  labs(
    x = "Year",
    y = "Number of Publications"
  ) +
  scale_x_continuous(
    limits = c(1973, 2026),  # Linha vai até 2025, mas não mostra 2025 no eixo
    breaks = c(seq(1973, 2024, by = 10)), # Ticks a cada 10 anos até 2024
    expand = c(0, 0)  # Remove a extensão além dos limites
  ) +
  scale_y_continuous(
    limits = c(0, NA),  # Eixo Y começa em 0 e o limite superior é ajustado automaticamente
    breaks = seq(0, max(publicacoes_por_ano$quantidade, na.rm = TRUE), by = 5), # Ticks no eixo y a cada 5
    expand = c(0, 0)  # Remove a extensão além dos limites
  ) +
  theme(
    panel.background = element_blank(), # Remove o fundo
    axis.line.x = element_line(color = "black", linewidth = 0.5), # Linha do eixo x preta
    axis.line.y = element_line(color = "black", linewidth = 0.5), # Linha do eixo y preta
    axis.ticks = element_line(color = "black"), # Ticks pretos nos números
    axis.text = element_text(color = "black"), # Texto dos eixos preto
    plot.title = element_text(hjust = 0.5) # Centraliza o título
  )