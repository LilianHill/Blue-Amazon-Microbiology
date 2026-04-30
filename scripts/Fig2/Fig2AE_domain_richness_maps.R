# Criado por Lilian Hill em 12/08/25
# ==============================================================================
# ANÁLISE COMPARATIVA POR DOMÍNIOS MICROBIOLÓGICOS
# Virus, Bacteria, Archaea, Fungi
# VERSÃO COM ÁREAS OCEÂNICAS DA AMAZÔNIA AZUL
# ==============================================================================

# Carregar bibliotecas necessárias
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(gridExtra)
library(ggalluvial)
library(treemapify)
library(forcats)
library(scales)
library(stringr)    # Para str_detect() e outras funções de string
library(readxl)     # Para read_excel()
library(sf)         # Para trabalhar com shapefiles

# Carregar dados
file_path <- 'C:/Users/lilia/OneDrive/Documentos/LECOM/INCT-BAA/spp_inct_ecor.xlsx'
dados <- read_excel(file_path)

# ==============================================================================
# 1. PREPARAÇÃO DOS DADOS POR DOMÍNIO
# ==============================================================================

# Verificar e limpar dados de domínio
cat("Verificando dados de domínio...\n")
print(table(dados$Kingdom_Domain, useNA = "always"))

# Padronizar nomes dos domínios
dados_dominios <- dados %>%
  filter(!is.na(Kingdom_Domain) & !is.na(Species)) %>%
  mutate(
    Dominio = case_when(
      str_detect(tolower(Kingdom_Domain), "virus|viral") ~ "Virus",
      str_detect(tolower(Kingdom_Domain), "bacteria|bacterial") ~ "Bacteria", 
      str_detect(tolower(Kingdom_Domain), "archaea|archaeal") ~ "Archaea",
      str_detect(tolower(Kingdom_Domain), "fungi|fungal|eukary") ~ "Fungi",
      TRUE ~ Kingdom_Domain
    )
  ) %>%
  filter(Dominio %in% c("Virus", "Bacteria", "Archaea", "Fungi"))

cat("Domínios após padronização:\n")
print(table(dados_dominios$Dominio))

# ==============================================================================
# 2. ANÁLISE COMPARATIVA ENTRE DOMÍNIOS
# ==============================================================================

# Estatísticas por domínio
stats_dominios <- dados_dominios %>%
  group_by(Dominio) %>%
  summarise(
    num_especies = n_distinct(Species, na.rm = TRUE),
    num_filos = n_distinct(Phylum, na.rm = TRUE),
    num_familias = n_distinct(Family, na.rm = TRUE),
    num_estudos = n_distinct(Number, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    prop_especies = num_especies / sum(num_especies),
    especies_por_estudo = num_especies / num_estudos,
    diversidade_filos = num_filos / num_especies
  )

# GRÁFICO 1: Comparação geral entre domínios
stats_long <- stats_dominios %>%
  select(Dominio, num_especies, num_filos, num_familias, num_estudos) %>%
  pivot_longer(cols = -Dominio, names_to = "Metrica", values_to = "Valor") %>%
  mutate(
    Metrica = case_when(
      Metrica == "num_especies" ~ "Espécies",
      Metrica == "num_filos" ~ "Filos", 
      Metrica == "num_familias" ~ "Famílias",
      Metrica == "num_estudos" ~ "Estudos"
    )
  )

p_comparacao_dominios <- ggplot(stats_long, aes(x = Dominio, y = Valor, fill = Dominio)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~Metrica, scales = "free_y", ncol = 2) +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title = "Comparação entre Domínios Microbiológicos",
    subtitle = "Diversidade taxonômica e esforço de pesquisa",
    x = "Domínio",
    y = "Número",
    fill = "Domínio"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 11, face = "bold")
  )

# GRÁFICO 2: Proporções circulares
p_prop_especies <- ggplot(stats_dominios, aes(x = "", y = prop_especies, fill = Dominio)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_viridis_d() +
  labs(
    title = "Proporção de Espécies por Domínio",
    fill = "Domínio"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# GRÁFICO 3: Eficiência de pesquisa
p_eficiencia <- ggplot(stats_dominios, aes(x = num_estudos, y = num_especies, 
                                           size = num_filos, color = Dominio)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = Dominio), hjust = 0, vjust = -1, size = 4) +
  scale_color_viridis_d() +
  scale_size_continuous(name = "Número\nde Filos", range = c(5, 15)) +
  labs(
    title = "Eficiência de Pesquisa por Domínio",
    subtitle = "Número de espécies descobertas vs. esforço de pesquisa",
    x = "Número de Estudos",
    y = "Número de Espécies",
    color = "Domínio"
  ) +
  theme_minimal()

# ==============================================================================
# 3. ANÁLISE DOS TÁXONS MAIS ABUNDANTES POR DOMÍNIO
# ==============================================================================

# Top filos por domínio
top_filos_dominio <- dados_dominios %>%
  group_by(Dominio, Phylum) %>%
  summarise(
    num_especies = n_distinct(Species, na.rm = TRUE),
    num_estudos = n_distinct(Number, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(Dominio) %>%
  arrange(desc(num_especies)) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  mutate(
    Phylum_label = paste0(Phylum, " (", num_especies, " spp.)")
  )

# GRÁFICO 4: Top 5 filos por domínio
p_top_filos <- ggplot(top_filos_dominio, aes(x = reorder(Phylum, num_especies), 
                                             y = num_especies, fill = Dominio)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~Dominio, scales = "free", ncol = 2) +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(
    title = "Top 5 Filos Mais Diversos por Domínio",
    x = "Filo",
    y = "Número de Espécies",
    fill = "Domínio"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "none"
  )

# Top famílias por domínio
top_familias_dominio <- dados_dominios %>%
  filter(!is.na(Family) & Family != "") %>%
  group_by(Dominio, Family) %>%
  summarise(
    num_especies = n_distinct(Species, na.rm = TRUE),
    num_estudos = n_distinct(Number, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(Dominio) %>%
  arrange(desc(num_especies)) %>%
  slice_head(n = 10) %>%
  ungroup()

# GRÁFICO 5: Treemap das famílias mais diversas
p_treemap_familias <- ggplot(top_familias_dominio, 
                             aes(area = num_especies, fill = Dominio, 
                                 label = paste(Family, "\n", num_especies, "spp."))) +
  geom_treemap(alpha = 0.8, color = "white", size = 2) +
  geom_treemap_text(color = "white", place = "center", size = 8) +
  facet_wrap(~Dominio, ncol = 2) +
  scale_fill_viridis_d() +
  labs(
    title = "Top 10 Famílias Mais Diversas por Domínio",
    subtitle = "Tamanho proporcional ao número de espécies"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

# ==============================================================================
# 4. ANÁLISE TEMPORAL POR DOMÍNIO
# ==============================================================================

# Tendências temporais
tendencias_dominios <- dados_dominios %>%
  filter(!is.na(Date_year) & Date_year >= 1990) %>%
  group_by(Date_year, Dominio) %>%
  summarise(
    especies_anuais = n_distinct(Species, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(Dominio) %>%
  arrange(Date_year) %>%
  mutate(
    especies_acumuladas = cumsum(especies_anuais)
  ) %>%
  ungroup()

# GRÁFICO 6: Curvas de descoberta por domínio
p_temporal_dominios <- ggplot(tendencias_dominios, 
                              aes(x = Date_year, y = especies_acumuladas, 
                                  color = Dominio, group = Dominio)) +
  geom_line(size = 1.5, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_viridis_d() +
  labs(
    title = "Curvas de Descoberta de Espécies por Domínio",
    subtitle = "Acumulação temporal de espécies descritas",
    x = "Ano",
    y = "Número Acumulado de Espécies",
    color = "Domínio"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ==============================================================================
# 5. ANÁLISE GEOGRÁFICA COM SHAPEFILES OCEÂNICOS
# ==============================================================================

# Verificar se existem dados geográficos
if("Latitude" %in% colnames(dados_dominios) & "Longitude" %in% colnames(dados_dominios)) {
  
  cat("Preparando análise geográfica por domínios...\n")
  
  # Processar dados geográficos
  geo_dominios <- dados_dominios %>%
    filter(!is.na(Latitude) & !is.na(Longitude)) %>%
    # Converter para numérico caso não seja
    mutate(
      Latitude = as.numeric(as.character(Latitude)),
      Longitude = as.numeric(as.character(Longitude))
    ) %>%
    filter(!is.na(Latitude) & !is.na(Longitude)) %>%
    filter(Latitude >= -90 & Latitude <= 90 & Longitude >= -180 & Longitude <= 180) %>%
    mutate(
      lat_grid = round(Latitude / 0.5) * 0.5,
      lon_grid = round(Longitude / 0.5) * 0.5
    ) %>%
    group_by(lat_grid, lon_grid, Dominio) %>%
    summarise(
      num_especies = n_distinct(Species, na.rm = TRUE),
      num_estudos = n_distinct(Number, na.rm = TRUE),
      especies_principais = paste(head(unique(Species), 3), collapse = "; "),
      .groups = 'drop'
    ) %>%
    filter(lat_grid >= -35 & lat_grid <= 10 & 
             lon_grid >= -75 & lon_grid <= -30)
  
  # ===========================================================================
  # CARREGAR SHAPEFILES
  # ===========================================================================
  
  cat("Carregando shapefiles...\n")
  base_path <- "C:/Users/lilia/OneDrive/Documentos/LECOM/INCT-BAA/shapefiles/"
  
  # Função para carregar shapefile com tratamento de erro
  carregar_shapefile <- function(caminho, nome) {
    tryCatch({
      sf_obj <- st_read(caminho, quiet = TRUE)
      if (st_crs(sf_obj)$epsg != 4326) {
        sf_obj <- st_transform(sf_obj, 4326)
      }
      cat("✓", nome, "carregado com sucesso!\n")
      return(sf_obj)
    }, error = function(e) {
      cat("⚠ Erro ao carregar", nome, ":", e$message, "\n")
      return(NULL)
    })
  }
  
  # Carregar shapefiles básicos
  ecorregioes <- carregar_shapefile(
    paste0(base_path, "Shapefile_Ecorregioes/ecorregioes.shp"), 
    "Ecorregiões"
  )
  
  estados_brasil <- carregar_shapefile(
    paste0(base_path, "Shapefile_estados_brasileiros/estados_br_2007.shp"), 
    "Estados brasileiros"
  )
  
  mundo <- carregar_shapefile(
    paste0(base_path, "Shapefile_mundo/shape_mundo.shp"), 
    "Mundo"
  )
  
  # Carregar os 4 shapefiles oceânicos da Amazônia Azul
  cat("Carregando shapefiles oceânicos da Amazônia Azul...\n")
  
  north_oceanic <- carregar_shapefile(
    paste0(base_path, "North_Oceanic.shp"), 
    "North Oceanic"
  )
  
  rio_grande_rise <- carregar_shapefile(
    paste0(base_path, "Rio_Grande_Rise.shp"), 
    "Rio Grande Rise"
  )
  
  south_east_oceanic <- carregar_shapefile(
    paste0(base_path, "South_East_Oceanic.shp"), 
    "South East Oceanic"
  )
  
  south_oceanic <- carregar_shapefile(
    paste0(base_path, "South_Oceanic.shp"), 
    "South Oceanic"
  )
  
  # Limites geográficos expandidos
  brasil_bbox <- list(xmin = -75, xmax = -25, ymin = -40, ymax = 10)
  
  # ===========================================================================
  # MAPA 1: DISTRIBUIÇÃO INTEGRADA
  # ===========================================================================
  
  p_mapa_integrado <- ggplot() +
    # Base do mundo
    {if(!is.null(mundo)) 
      geom_sf(data = mundo, fill = "lightgray", color = "white", size = 0.1)} +
    
    # Ecorregiões
    {if(!is.null(ecorregioes)) 
      geom_sf(data = ecorregioes, fill = "lightgreen", color = "darkgreen", alpha = 0.3, size = 0.2)} +
    # Áreas oceânicas
    {if(!is.null(north_oceanic)) 
      geom_sf(data = north_oceanic, fill = "steelblue", color = "darkblue", alpha = 0.4, size = 0.3)} +
    
    {if(!is.null(rio_grande_rise)) 
      geom_sf(data = rio_grande_rise, fill = "lightblue", color = "darkblue", alpha = 0.4, size = 0.3)} +
    
    {if(!is.null(south_east_oceanic)) 
      geom_sf(data = south_east_oceanic, fill = "cornflowerblue", color = "darkblue", alpha = 0.4, size = 0.3)} +
    
    {if(!is.null(south_oceanic)) 
      geom_sf(data = south_oceanic, fill = "blue", color = "darkslateblue", alpha = 0.4, size = 0.3)} +
    
    # Estados brasileiros
    {if(!is.null(estados_brasil)) 
      geom_sf(data = estados_brasil, fill = "lightgray", color = "white", size = 0.3)} +
    
    # Pontos por domínio
    geom_point(data = geo_dominios, 
               aes(x = lon_grid, y = lat_grid, 
                   size = num_especies, 
                   color = Dominio),
               alpha = 0.8) +
    scale_color_viridis_d() +
    scale_size_continuous(name = "Riqueza\nEspécies", range = c(1, 10), trans = "log10") +
    coord_sf(xlim = c(brasil_bbox$xmin, brasil_bbox$xmax), 
             ylim = c(brasil_bbox$ymin, brasil_bbox$ymax)) +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    ) +
    labs(
      title = "Distribuição Geográfica por Domínio Microbiológico",
      subtitle = "Contexto: Ecorregiões + Áreas Oceânicas da Amazônia Azul",
      color = "Domínio"
    )
  
  # ===========================================================================
  # MAPA 2: FACETAS POR DOMÍNIO
  # ===========================================================================
  
  p_mapa_facetas <- ggplot() +
    # Base do mundo
    {if(!is.null(mundo)) 
      geom_sf(data = mundo, fill = "lightgray", color = "white", size = 0.1)} +
  
    # Ecorregiões
    {if(!is.null(ecorregioes)) 
      geom_sf(data = ecorregioes, fill = "lightgreen", color = "darkgreen", alpha = 0.3, size = 0.2)} +
    # Áreas oceânicas
    {if(!is.null(north_oceanic)) 
      geom_sf(data = north_oceanic, fill = "steelblue", color = "darkblue", alpha = 0.4, size = 0.3)} +
    
    {if(!is.null(rio_grande_rise)) 
      geom_sf(data = rio_grande_rise, fill = "lightblue", color = "darkblue", alpha = 0.4, size = 0.3)} +
    
    {if(!is.null(south_east_oceanic)) 
      geom_sf(data = south_east_oceanic, fill = "cornflowerblue", color = "darkblue", alpha = 0.4, size = 0.3)} +
    
    {if(!is.null(south_oceanic)) 
      geom_sf(data = south_oceanic, fill = "blue", color = "darkslateblue", alpha = 0.4, size = 0.3)} +
    
    # Estados brasileiros
    {if(!is.null(estados_brasil)) 
      geom_sf(data = estados_brasil, fill = "lightgray", color = "white", size = 0.3)} +
    
    # Pontos por domínio
    geom_point(data = geo_dominios, 
               aes(x = lon_grid, y = lat_grid, 
                   size = num_especies, 
                   color = num_especies),
               alpha = 0.8) +
    facet_wrap(~Dominio, ncol = 2) +
    scale_color_viridis_c(name = "Riqueza\nEspécies", trans = "log10") +
    scale_size_continuous(name = "Riqueza\nEspécies", range = c(1, 8), trans = "log10") +
    coord_sf(xlim = c(brasil_bbox$xmin, brasil_bbox$xmax), 
             ylim = c(brasil_bbox$ymin, brasil_bbox$ymax)) +
    theme_void() +
    theme(
      strip.text = element_text(size = 11, face = "bold"),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 14)
    ) +
    labs(
      title = "Distribuição Específica por Domínio",
      subtitle = "Áreas Oceânicas da Amazônia Azul"
    )
  
  # ===========================================================================
  # MAPA 3: DENSIDADE DE ESTUDOS
  # ===========================================================================
  
  p_mapa_estudos <- ggplot() +
    # Base do mundo
    {if(!is.null(mundo)) 
      geom_sf(data = mundo, fill = "lightgray", color = "white", size = 0.1)} +
    
    # Ecorregiões
    {if(!is.null(ecorregioes)) 
      geom_sf(data = ecorregioes, fill = "lightgreen", color = "darkgreen", alpha = 0.3, size = 0.2)} +
    # Áreas oceânicas
    {if(!is.null(north_oceanic)) 
      geom_sf(data = north_oceanic, fill = "steelblue", color = "darkblue", alpha = 0.4, size = 0.3)} +
    
    {if(!is.null(rio_grande_rise)) 
      geom_sf(data = rio_grande_rise, fill = "lightblue", color = "darkblue", alpha = 0.4, size = 0.3)} +
    
    {if(!is.null(south_east_oceanic)) 
      geom_sf(data = south_east_oceanic, fill = "cornflowerblue", color = "darkblue", alpha = 0.4, size = 0.3)} +
    
    {if(!is.null(south_oceanic)) 
      geom_sf(data = south_oceanic, fill = "blue", color = "darkslateblue", alpha = 0.4, size = 0.3)} +
    
    # Estados brasileiros
    {if(!is.null(estados_brasil)) 
      geom_sf(data = estados_brasil, fill = "lightgray", color = "white", size = 0.3)} +
    
    # Pontos de estudos
    geom_point(data = geo_dominios, 
               aes(x = lon_grid, y = lat_grid, 
                   size = num_estudos, 
                   color = Dominio),
               alpha = 0.8) +
    scale_color_viridis_d() +
    scale_size_continuous(name = "Número de\nEstudos", range = c(1, 10), trans = "log10") +
    coord_sf(xlim = c(brasil_bbox$xmin, brasil_bbox$xmax), 
             ylim = c(brasil_bbox$ymin, brasil_bbox$ymax)) +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    ) +
    labs(
      title = "Densidade de Estudos por Domínio",
      subtitle = "Destaque: Ecorregiões + Áreas Oceânicas",
      color = "Domínio"
    )
  
  # Exibir mapas
  cat("✓ Mapas geográficos gerados!\n")
  p_mapa_integrado
  p_mapa_facetas
  p_mapa_estudos
}

# ==============================================================================
# 6. EXIBIR GRÁFICOS PRINCIPAIS
# ==============================================================================

cat("Exibindo gráficos principais...\n")

# Exibir gráficos
p_comparacao_dominios
p_prop_especies
p_eficiencia
p_top_filos
p_treemap_familias
p_temporal_dominios
p_mapa_integrado
p_mapa_facetas
p_mapa_estudos

# ==============================================================================
# 7. SALVAR DADOS ESTATÍSTICOS
# ==============================================================================

cat("Salvando dados estatísticos...\n")

setwd('C:/Users/lilia/OneDrive/Documentos/LECOM/INCT-BAA')
write.csv(stats_dominios, "estatisticas_por_dominio.csv", row.names = FALSE)
write.csv(top_filos_dominio, "top_filos_por_dominio.csv", row.names = FALSE)
write.csv(top_familias_dominio, "top_familias_por_dominio.csv", row.names = FALSE)
write.csv(tendencias_dominios, "tendencias_temporais_dominios.csv", row.names = FALSE)

if(exists("geo_dominios")) {
  write.csv(geo_dominios, "distribuicao_geografica_dominios.csv", row.names = FALSE)
}

# ==============================================================================
# 8. RELATÓRIO FINAL
# ==============================================================================

cat("\n=== ANÁLISE POR DOMÍNIOS CONCLUÍDA ===\n")
cat("GRÁFICOS GERADOS:\n")
cat("✓ Comparação geral entre domínios\n")
cat("✓ Proporção de espécies por domínio\n")
cat("✓ Eficiência de pesquisa por domínio\n")
cat("✓ Top 5 filos mais diversos\n")
cat("✓ Treemap das famílias mais diversas\n")
cat("✓ Curvas de descoberta temporal\n")
if(exists("p_mapa_integrado")) {
  cat("✓ Mapas geográficos com áreas oceânicas\n")
}

cat("\nÁREAS OCEÂNICAS INCLUÍDAS:\n")
cat("✓ North Oceanic\n")
cat("✓ Rio Grande Rise\n")
cat("✓ South East Oceanic\n")
cat("✓ South Oceanic\n")

cat("\nDADOS SALVOS:\n")
cat("✓ estatisticas_por_dominio.csv\n")
cat("✓ top_filos_por_dominio.csv\n")
cat("✓ top_familias_por_dominio.csv\n")
cat("✓ tendencias_temporais_dominios.csv\n")
if(exists("geo_dominios")) {
  cat("✓ distribuicao_geografica_dominios.csv\n")
}

cat("\nESTATÍSTICAS RESUMO:\n")
print(stats_dominios)

cat("\n=== ANÁLISE COMPLETA ===\n")