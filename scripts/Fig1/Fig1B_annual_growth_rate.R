# Pacotes
library (readxl)
library(ggplot2)
library (dplyr)
library(tidyr)
library(zoo)
library(tm)
library(stringr)
library(openxlsx)
library(iNEXT)
library(ggthemes)
library(reshape2)

# Dados  #substituir pelos seus dados

# Ref
file_path <- 'C:/Users/lilia/OneDrive/Documentos/LECOM/INCT-BAA/ref_INCT.xlsx'
ref <- read_excel(file_path)

# Spp
file_path <- 'C:/Users/lilia/OneDrive/Documentos/LECOM/INCT-BAA/spp_INCT_ecor.xlsx'
spp <- read_excel(file_path)

#### 1. TREND ANUAL ######
# Contar o número de publicações por ano
publicacoes_por_ano <- ref %>%
  group_by(Year) %>%
  summarise(quantidade = n()) %>%
  arrange(Year) %>%
  filter(Year != 2024)  # Filtro para ano incompleto 

# # Calcular a taxa de crescimento anual
publicacoes_por_ano$growth_rate <- c(NA, diff(publicacoes_por_ano$quantidade) / head(publicacoes_por_ano$quantidade, -1) * 100)

# Calcular a média móvel da taxa de crescimento anual
publicacoes_por_ano <- publicacoes_por_ano %>%
  mutate(
    growth_rate_sma = rollmean(growth_rate, 3, fill = NA, align = "center"),
    growth_rate_color = ifelse(growth_rate < 0, "Negative", "Positive")  # Cor para barras negativas
  )

# Remover linhas com NA ou valores fora do intervalo (se necessário)
publicacoes_por_ano <- publicacoes_por_ano %>%
  filter(!is.na(growth_rate) & !is.na(growth_rate_sma))  # Remover linhas com NA

# Gerar o gráfico com barras de crescimento anual e a linha de média móvel
ggplot(publicacoes_por_ano, aes(x = Year)) +
  # Barras de crescimento anual (diferenciação de cor para negativo)
  geom_bar(aes(y = growth_rate, fill = growth_rate_color), stat = "identity", alpha = 0.7) +
  
  # Linha de média móvel da taxa de crescimento
  geom_line(aes(y = growth_rate_sma, color = "Moving Average"), linewidth = 0.5) +  # Linha preta mais fina
  
  # Adicionar a linha preta no valor 0
  geom_hline(yintercept = 0, color = "black", size = 0.5) +  # Linha preta mais fina
  
  # Ajustar rótulos e legenda
  labs(
    title = NULL,
    x = "Year",
    y = "Growth Rate (%)",
    fill = "Growth Rate",  # Legenda para as barras
    color = "Trend"        # Legenda para a linha
  ) +
  
  # Ajustes estéticos
  scale_fill_manual(values = c("Positive" = "lightblue", "Negative" = "#F1A7A7")) +  # Cor das barras (vermelho claro)
  scale_color_manual(values = c("Moving Average" = "#9FDAAD")) +  # Cor da linha de média móvel
  scale_y_continuous(expand = c(0, 0), breaks = seq(-1000, 1000, by = 50)) +  # Ajustar os ticks do eixo Y a cada 25%
  
  # Remover o fundo e adicionar linhas pretas aos eixos
  theme_minimal() +
  theme(
    legend.position.inside = c(0.1, 0.8),  # Coloca a legenda no canto superior esquerdo
    legend.direction = "vertical",  # Disposição vertical da legenda
    legend.spacing.y = unit(0.5, "lines"),  # Espaçamento entre os itens da legenda
    panel.grid = element_blank(),  # Remove a grade
    axis.line = element_line(color = "black"),  # Linha preta nos eixos
    axis.ticks = element_line(color = "black"),  # Ticks pretos nos eixos
    text = element_text(color = "black")  # Texto em preto
  )