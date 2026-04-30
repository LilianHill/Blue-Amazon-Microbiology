# criado por: Lilian Hill
# modificado em: 17/07/25
#-------------------------------------------------------------------------------
# Análises - MÉTODOS ##########################################
# Quais métodos são mais utilizados nos estudos? Análise de técnicas moleculares,
# morfológicas e outras abordagens metodológicas


library(tidyverse)
library(fmsb)         # pacote que contém a função radarchart()
library(tm)
library(readr)
library(openxlsx)
library(readxl)
library(stringr)      # para manipulação de strings
library(ggplot2)      # para gráficos adicionais
library(RColorBrewer) # para paletas de cores


file_path <- 'C:/Users/lilia/OneDrive/Documentos/LECOM/INCT-BAA/ref_INCT.xlsx'
ref <- read_excel(file_path)

# Importa a planilha com os dados dos trabalhos analisados
analises_metodos <- ref

#-------------------------------
# DEFININDO TERMOS DE INTERESSE
#-------------------------------

# Técnicas moleculares
molecular_terms <- c("dna", "rna", "16s", "18s", "cox1", "coi", "its", 
                     "metagenomic", "metagenomics", "metabarcoding", 
                     "barcoding", "pcr", "qpcr", "rt-pcr", "sequencing",
                     "genome", "genomic", "phylogenetic", "phylogeny",
                     "microsatellite", "snp", "aflp", "rapd", "rflp",
                     "next-generation", "ngs", "illumina", "sanger",
                     "environmental dna", "edna")

# Técnicas morfológicas/tradicionais
morphological_terms <- c("morphology", "morphological", "taxonomy", 
                         "taxonomic", "identification", "microscopy",
                         "morphometric", "biometric", "visual", "counting",
                         "stomach content", "gut content", "diet analysis",
                         "feeding", "food", "prey", "predator")

# Técnicas ecológicas/de campo
ecological_terms <- c("field", "sampling", "survey", "monitoring",
                      "transect", "quadrat", "trap", "net", "seine",
                      "trawl", "gillnet", "electrofishing", "capture",
                      "mark-recapture", "telemetry", "tracking",
                      "abundance", "density", "biomass", "diversity")

# Técnicas estatísticas/analíticas
statistical_terms <- c("statistical", "statistics", "multivariate",
                       "ordination", "pca", "nmds", "cluster", "anova",
                       "regression", "model", "modeling", "glm", "glmm",
                       "bayesian", "bootstrap", "permanova", "simper")

# Combina todos os termos
all_terms <- list(
  "Molecular" = molecular_terms,
  "Morphological" = morphological_terms,
  "Ecological" = ecological_terms,
  "Statistical" = statistical_terms
)

#-------------------------------
# TRABALHANDO OS DADOS - MÉTODOS
#-------------------------------

# Padroniza os textos dos métodos para minúsculas
metodos <- analises_metodos %>%
  mutate(Method = tolower(Method)) %>%
  filter(!is.na(Method) & Method != "")

# Função para verificar se algum termo está presente no método
check_terms <- function(method_text, terms_list) {
  if(is.na(method_text)) return(FALSE)
  return(any(str_detect(method_text, paste(terms_list, collapse = "|"))))
}

# Análise por categoria de método
results_by_category <- tibble()

for(category in names(all_terms)) {
  category_data <- metodos %>%
    mutate(has_term = map_lgl(Method, ~check_terms(.x, all_terms[[category]]))) %>%
    filter(has_term == TRUE) %>%
    select(Number, Method, has_term) %>%
    mutate(Category = category,
           n = 1)
  
  category_summary <- category_data %>%
    summarise(
      Category = category,
      count = n(),
      percentage = (n() / nrow(metodos)) * 100
    )
  
  results_by_category <- bind_rows(results_by_category, category_summary)
}

# Análise por termo específico (top termos moleculares)
molecular_specific <- tibble()

for(term in molecular_terms) {
  term_data <- metodos %>%
    mutate(has_term = str_detect(Method, term)) %>%
    filter(has_term == TRUE) %>%
    summarise(
      Term = term,
      count = n(),
      percentage = (n() / nrow(metodos)) * 100
    ) %>%
    filter(count > 0)
  
  molecular_specific <- bind_rows(molecular_specific, term_data)
}

# Ordena por frequência
molecular_specific <- molecular_specific %>%
  arrange(desc(count))

# Análise dos métodos mais frequentes (texto completo)
top_methods <- metodos %>%
  count(Method, sort = TRUE) %>%
  mutate(percentage = (n / sum(n)) * 100) %>%
  head(20)

#-------------------------------
# ESTATÍSTICAS DESCRITIVAS
#-------------------------------

cat("=== RESUMO DA ANÁLISE DE MÉTODOS ===\n")
cat("Total de estudos analisados:", nrow(metodos), "\n")
cat("Estudos com informação de método:", sum(!is.na(metodos$Method)), "\n\n")

cat("=== MÉTODOS POR CATEGORIA ===\n")
print(results_by_category)

cat("\n=== TOP 10 TERMOS MOLECULARES ===\n")
print(head(molecular_specific, 10))

cat("\n=== TOP 10 MÉTODOS MAIS FREQUENTES ===\n")
print(head(top_methods, 10))

# FIGURAS ----------------------------------------------------------------------------

# ---------------------------------------------
#  Gráfico de Barras - Métodos por Categoria
# ---------------------------------------------

# Gráfico de barras para categorias
p1 <- ggplot(results_by_category, aes(x = reorder(Category, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Distribution of Methods by Category",
       x = "Method Category",
       y = "Number of Studies",
       caption = "Baseado na análise de termos-chave nos métodos") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

print(p1)

# ---------------------------------------------
#  Gráfico de Barras - Top Termos Moleculares
# ---------------------------------------------

if(nrow(molecular_specific) > 0) {
  p2 <- ggplot(head(molecular_specific, 10), aes(x = reorder(Term, count), y = count)) +
    geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
    geom_text(aes(label = paste0(count, " (", round(percentage, 1), "%)")), 
              hjust = -0.1, size = 3) +
    coord_flip() +
    labs(title = "Top 10 Most Frequently Used Molecular Terms",
         x = "Molecular Term",
         y = "Number of Studies",
         caption = "Frequência de termos relacionados a técnicas moleculares") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  
  print(p2)
}

# ---------------------------------------------
#  Gráfico Radar Chart - Comparação de Categorias
# ---------------------------------------------

# Prepara dados para o radar chart
radar_data <- results_by_category %>%
  select(Category, percentage) %>%
  pivot_wider(names_from = Category, values_from = percentage, values_fill = 0)

# Adiciona valores máximos e mínimos
max_val <- max(results_by_category$percentage, na.rm = TRUE)
radar_data <- rbind(radar_data, 
                    setNames(rep(max_val, ncol(radar_data)), names(radar_data)))  # máximo
radar_data <- rbind(radar_data, 
                    setNames(rep(0, ncol(radar_data)), names(radar_data)))       # mínimo

# Reorganiza para colocar max e min no topo
radar_data <- radar_data[c(2, 3, 1), ]

# Cria o radar chart
colors_border = c(rgb(0.2, 0.5, 0.5, 0.9))
colors_in     = c(rgb(0.2, 0.5, 0.5, 0.4))

radarchart(radar_data, axistype = 1,
           pcol = colors_border, pfcol = colors_in, plwd = 4, plty = 1,
           cglcol = "grey", cglty = 1, axislabcol = "grey", 
           caxislabels = seq(0, ceiling(max_val), ceiling(max_val/4)), 
           cglwd = 1.1, vlcex = 0.8)

# Legenda
legend(x = 0.7, y = 1.3,
       legend = c("Distribuição de Métodos"),
       bty = "n", pch = 20, col = colors_border,
       text.col = "black", cex = 0.9, pt.cex = 1.6)

# ---------------------------------------------
#  Análise Adicional - Métodos Combinados
# ---------------------------------------------

# Verifica estudos que usam múltiplas categorias
multi_method_analysis <- metodos %>%
  rowwise() %>%
  mutate(
    has_molecular = check_terms(Method, all_terms[["Molecular"]]),
    has_morphological = check_terms(Method, all_terms[["Morphological"]]),
    has_ecological = check_terms(Method, all_terms[["Ecological"]]),
    has_statistical = check_terms(Method, all_terms[["Statistical"]])
  ) %>%
  mutate(
    method_count = sum(c(has_molecular, has_morphological, has_ecological, has_statistical)),
    method_combination = case_when(
      method_count == 1 ~ "Método Único",
      method_count == 2 ~ "Dois Métodos",
      method_count == 3 ~ "Três Métodos",
      method_count >= 4 ~ "Quatro ou Mais Métodos",
      TRUE ~ "Nenhum Método Identificado"
    )
  ) %>%
  ungroup()

# Resumo da análise combinada
combination_summary <- multi_method_analysis %>%
  count(method_combination, sort = TRUE) %>%
  mutate(percentage = (n / sum(n)) * 100)

cat("\n=== ANÁLISE DE MÉTODOS COMBINADOS ===\n")
print(combination_summary)

# Gráfico de pizza para métodos combinados
p3 <- ggplot(combination_summary, aes(x = "", y = n, fill = method_combination)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição de Estudos por Número de Métodos Utilizados",
       fill = "Combinação de Métodos") +
  theme_void() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Set3")

print(p3)

# Salva os resultados em arquivo
write.xlsx(list(
  "Resumo_Categorias" = results_by_category,
  "Termos_Moleculares" = molecular_specific,
  "Top_Metodos" = top_methods,
  "Metodos_Combinados" = combination_summary,
  "Dados_Detalhados" = multi_method_analysis
), "analise_metodos_resultados.xlsx")

cat("\n=== ANÁLISE CONCLUÍDA ===\n")
cat("Resultados salvos em: analise_metodos_resultados.xlsx\n")