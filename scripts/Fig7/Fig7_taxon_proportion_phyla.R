# criado por: Lilian Hill
# modificado em: 11/08/25
#-------------------------------------------------------------------------------
# Análises - Proporção táxons por número de estudos ##########################################


# Carregar bibliotecas necessárias
library(ggplot2)
library(dplyr)
library(readxl) # ou readr se for CSV

# Ler os dados da planilha
# Substitua 'caminho_para_sua_planilha.xlsx' pelo caminho real do seu arquivo
# Se for CSV, use: dados <- read.csv("caminho_para_sua_planilha.csv")
dados <- file_path <- 'C:/Users/lilia/OneDrive/Documentos/LECOM/INCT-BAA/spp_inct_ecor.xlsx'
dados <- read_excel(file_path)

# Processar os dados para criar o gráfico
dados_processados <- dados %>%
  # Filtrar dados válidos
  filter(!is.na(Phylum) & !is.na(Species)) %>%
  
  # Agrupar por filo e calcular métricas
  group_by(Phylum) %>%
  summarise(
    num_estudos = n_distinct(Number, na.rm = TRUE),
    num_especies = n_distinct(Species, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  
  # Calcular a proporção de espécies
  mutate(
    proporcao_especies = num_especies / sum(num_especies, na.rm = TRUE)
  ) %>%
  
  # Filtrar filos com pelo menos alguns estudos para melhor visualização
  filter(num_estudos >= 5)

# Criar o gráfico
p <- ggplot(dados_processados, aes(x = num_estudos, y = proporcao_especies)) +
  
  # Adicionar pontos
  geom_point(size = 2, alpha = 0.7, color = "black") +
  
  # Adicionar rótulos dos filos
  geom_text(aes(label = Phylum), 
            hjust = 0, vjust = -0.5, 
            size = 3, 
            check_overlap = TRUE) +
  
  # Configurar escalas
  scale_x_continuous(
    name = "Number of studies",
    limits = c(0, max(dados_processados$num_estudos) * 1.1),
    expand = c(0.02, 0)
  ) +
  
  scale_y_continuous(
    name = "Proportion of species",
    limits = c(0, max(dados_processados$proporcao_especies) * 1.1),
    expand = c(0.02, 0)
  ) +
  
  # Aplicar tema limpo
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 11),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Exibir o gráfico
print(p)

# Salvar o gráfico (opcional)
ggsave("proporcao_especies_estudos.png", 
       plot = p, 
       width = 8, 
       height = 6, 
       dpi = 300, 
       bg = "white")

# Visualizar os dados processados
print("Dados processados:")
print(dados_processados)

# Estatísticas resumo
cat("\nResumo dos dados:\n")
cat("Número total de filos:", nrow(dados_processados), "\n")
cat("Número total de estudos:", sum(dados_processados$num_estudos), "\n")
cat("Número total de espécies:", sum(dados_processados$num_especies), "\n")

# Carregar bibliotecas necessárias
library(ggplot2)
library(dplyr)
library(readxl) # ou readr se for CSV

# Ler os dados da planilha
# Substitua 'caminho_para_sua_planilha.xlsx' pelo caminho real do seu arquivo
# Se for CSV, use: dados <- read.csv("caminho_para_sua_planilha.csv")
dados <- read_excel("caminho_para_sua_planilha.xlsx")

# Processar os dados para criar o gráfico
dados_processados <- dados %>%
  # Filtrar dados válidos
  filter(!is.na(Phylum) & !is.na(Species)) %>%
  
  # Agrupar por filo e calcular métricas
  group_by(Phylum) %>%
  summarise(
    num_estudos = n_distinct(Number_Citation, na.rm = TRUE),
    num_especies = n_distinct(Species, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  
  # Calcular a proporção de espécies
  mutate(
    proporcao_especies = num_especies / sum(num_especies, na.rm = TRUE)
  ) %>%
  
  # Filtrar filos com pelo menos alguns estudos para melhor visualização
  filter(num_estudos >= 5)

# Criar o gráfico
p <- ggplot(dados_processados, aes(x = num_estudos, y = proporcao_especies)) +
  
  # Adicionar pontos
  geom_point(size = 2, alpha = 0.7, color = "black") +
  
  # Adicionar rótulos dos filos
  geom_text(aes(label = Phylum), 
            hjust = 0, vjust = -0.5, 
            size = 3, 
            check_overlap = TRUE) +
  
  # Configurar escalas
  scale_x_continuous(
    name = "Number of studies",
    limits = c(0, max(dados_processados$num_estudos) * 1.1),
    expand = c(0.02, 0)
  ) +
  
  scale_y_continuous(
    name = "Proportion of species",
    limits = c(0, max(dados_processados$proporcao_especies) * 1.1),
    expand = c(0.02, 0)
  ) +
  
  # Aplicar tema limpo
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 11),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Exibir o gráfico
print(p)

# Salvar o gráfico de filos (opcional)
ggsave("proporcao_especies_estudos_filos.png", 
       plot = p, 
       width = 8, 
       height = 6, 
       dpi = 300, 
       bg = "white")

# =============================================================================
# GRÁFICO PARA FAMÍLIAS
# =============================================================================

# Processar os dados para famílias
dados_familias <- dados %>%
  # Filtrar dados válidos
  filter(!is.na(Family) & !is.na(Species) & Family != "" & Family != "NA") %>%
  
  # Agrupar por família e calcular métricas
  group_by(Family) %>%
  summarise(
    num_estudos = n_distinct(Number, na.rm = TRUE),
    num_especies = n_distinct(Species, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  
  # Calcular a proporção de espécies
  mutate(
    proporcao_especies = num_especies / sum(num_especies, na.rm = TRUE)
  ) %>%
  
  # Filtrar famílias com pelo menos alguns estudos para melhor visualização
  filter(num_estudos >= 3) %>%
  
  # Ordenar por número de estudos para facilitar visualização
  arrange(desc(num_estudos))

# Criar o gráfico para famílias
p_familias <- ggplot(dados_familias, aes(x = num_estudos, y = proporcao_especies)) +
  
  # Adicionar pontos
  geom_point(size = 2, alpha = 0.7, color = "black") +
  
  # Adicionar rótulos das famílias
  geom_text(aes(label = Family), 
            hjust = 0, vjust = -0.5, 
            size = 2.5, 
            check_overlap = TRUE) +
  
  # Configurar escalas
  scale_x_continuous(
    name = "Number of studies",
    limits = c(0, max(dados_familias$num_estudos) * 1.1),
    expand = c(0.02, 0)
  ) +
  
  scale_y_continuous(
    name = "Proportion of species",
    limits = c(0, max(dados_familias$proporcao_especies) * 1.1),
    expand = c(0.02, 0)
  ) +
  
  # Aplicar tema limpo
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 11),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Adicionar título para diferenciação
  ggtitle("Análise por Famílias")

# Exibir o gráfico de famílias
print(p_familias)

# Salvar o gráfico de famílias
ggsave("proporcao_especies_estudos_familias.png", 
       plot = p_familias, 
       width = 10, 
       height = 7, 
       dpi = 300, 
       bg = "white")

# =============================================================================
# COMPARAÇÃO LADO A LADO (OPCIONAL)
# =============================================================================

library(gridExtra)

# Criar gráfico combinado
p_filos_titulo <- p + ggtitle("Análise por Filos")

# Combinar os gráficos lado a lado
p_combinado <- grid.arrange(p_filos_titulo, p_familias, ncol = 2)

# Salvar gráfico combinado
ggsave("comparacao_filos_familias.png", 
       plot = p_combinado, 
       width = 16, 
       height = 7, 
       dpi = 300, 
       bg = "white")

# =============================================================================
# ESTATÍSTICAS E RESUMOS
# =============================================================================

# Visualizar os dados processados
cat("=== DADOS PROCESSADOS - FILOS ===\n")
print(dados_processados)

cat("\n=== DADOS PROCESSADOS - FAMÍLIAS ===\n")
print(dados_familias)

# Estatísticas resumo
cat("\n=== RESUMO DOS DADOS ===\n")
cat("FILOS:\n")
cat("  Número total de filos:", nrow(dados_processados), "\n")
cat("  Número total de estudos (filos):", sum(dados_processados$num_estudos), "\n")
cat("  Número total de espécies (filos):", sum(dados_processados$num_especies), "\n")

cat("\nFAMÍLIAS:\n")
cat("  Número total de famílias:", nrow(dados_familias), "\n")
cat("  Número total de estudos (famílias):", sum(dados_familias$num_estudos), "\n")
cat("  Número total de espécies (famílias):", sum(dados_familias$num_especies), "\n")

# Top 10 famílias por número de estudos
cat("\n=== TOP 10 FAMÍLIAS POR NÚMERO DE ESTUDOS ===\n")
top_familias <- dados_familias %>%
  arrange(desc(num_estudos)) %>%
  head(10)
print(top_familias)

# =============================================================================
# SALVANDO DADOS EM ARQUIVOS
# =============================================================================

setwd("C:/Users/lilia/OneDrive/Documentos/LECOM/INCT-BAA/")

# 1. Salvar dados processados de filos
write.csv(dados_processados, 
          "dados_estatisticos_filos.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

# 2. Salvar dados processados de famílias
write.csv(dados_familias, 
          "dados_estatisticos_familias.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

# 3. Criar tabela de estatísticas resumo
estatisticas_resumo <- data.frame(
  Nivel_Taxonomico = c("Filos", "Famílias"),
  Numero_de_Taxa = c(nrow(dados_processados), nrow(dados_familias)),
  Total_Estudos = c(sum(dados_processados$num_estudos), sum(dados_familias$num_estudos)),
  Total_Especies = c(sum(dados_processados$num_especies), sum(dados_familias$num_especies)),
  Media_Estudos_por_Taxa = c(mean(dados_processados$num_estudos), mean(dados_familias$num_estudos)),
  Media_Especies_por_Taxa = c(mean(dados_processados$num_especies), mean(dados_familias$num_especies)),
  Taxa_mais_estudado = c(dados_processados$Phylum[which.max(dados_processados$num_estudos)],
                         dados_familias$Family[which.max(dados_familias$num_estudos)]),
  Max_num_estudos = c(max(dados_processados$num_estudos), max(dados_familias$num_estudos))
)

# Salvar tabela de estatísticas resumo
write.csv(estatisticas_resumo, 
          "estatisticas_resumo_geral.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

# 4. Criar e salvar top rankings
top_10_filos <- dados_processados %>%
  arrange(desc(num_estudos)) %>%
  head(10) %>%
  mutate(ranking = 1:n())

top_10_familias <- dados_familias %>%
  arrange(desc(num_estudos)) %>%
  head(10) %>%
  mutate(ranking = 1:n())

# Salvar rankings
write.csv(top_10_filos, 
          "top_10_filos_mais_estudados.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

write.csv(top_10_familias, 
          "top_10_familias_mais_estudadas.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

