library(dplyr)
library(tidyr) # Para pivot_wider()
library(purrr) # Para reduce()
library(ggplot2)
library(scales) # Para scales::hue_pal()

# --- Supondo que os dataframes e matrizes necessários já estão na memória ---
# - matriz_sim_cosseno_total (a matriz completa de similaridade entre todas as músicas)
# - data_dp (dataframe com ID, Grupos_MM_DP, etc. - sua base principal com os grupos)
# - resumo_topicos_estudo (o resumo de tópicos)

# --- 1. Recriar um dataframe longo com TODAS as similaridades de todos os pares de tópicos ---
# Esta lógica garante que todos os pares de similaridade cruzada existam.
cat("Calculando todas as similaridades de cosseno entre pares de tópicos...\n")

lista_similaridades <- list()
topicos_existentes <- sort(unique(data_dp$Grupos_MM_DP))

# --- NOVO PASSO: Identificar IDs Válidos da DTM ---
# Apenas IDs que estão na DTM podem ser usados para indexar a matriz de similaridade
codigos_validos_matriz <- row.names(matriz_sim_cosseno_total)


for (topico_i in topicos_existentes) {
  # Apenas pegar os IDs das músicas do tópico i que são Códigos válidos
  codigos_topico_i <- data_dp %>%
    filter(Grupos_MM_DP == topico_i) %>%
    pull(ID) %>%
    as.character()
  
  # Filtrar para garantir que o código realmente existe na matriz
  codigos_topico_i <- codigos_topico_i[codigos_topico_i %in% codigos_validos_matriz]
  
  for (topico_j in topicos_existentes) {
    codigos_topico_j <- data_dp %>%
      filter(Grupos_MM_DP == topico_j) %>%
      pull(ID) %>%
      as.character()
    
    codigos_topico_j <- codigos_topico_j[codigos_topico_j %in% codigos_validos_matriz]
    
    if (length(codigos_topico_i) > 0 & length(codigos_topico_j) > 0) {
      sub_matriz_sim <- matriz_sim_cosseno_total[codigos_topico_i, codigos_topico_j]
      
      if (topico_i == topico_j) {
        # Para o mesmo tópico, pegamos a triangular superior (similaridade interna)
        sim_scores <- sub_matriz_sim[upper.tri(sub_matriz_sim)]
      } else {
        # Para tópicos diferentes, pegamos todas as similaridades (similaridade cruzada)
        sim_scores <- as.vector(sub_matriz_sim)
      }
      
      lista_similaridades[[paste(topico_i, topico_j, sep = "_")]] <- tibble(
        topico_1 = topico_i,
        topico_2 = topico_j,
        similaridade = sim_scores
      )
    }
  }
}

df_similaridades_completo <- bind_rows(lista_similaridades)

if (nrow(df_similaridades_completo) == 0) {
  stop("Nenhum score de similaridade foi calculado. Verifique a matriz de similaridade total e a atribuição de tópicos.")
}
cat("Dataframe de similaridades completo criado com sucesso.\n")

# --- NOVO PASSO: Filtrar df_similaridades_completo para incluir APENAS similaridades INTERNAS ---
analise_similaridade_resumo_interna <- df_similaridades_completo %>%
  filter(topico_1 == topico_2) %>% # Filtra para que topico_1 e topico_2 sejam iguais
  group_by(topico_1) %>%
  summarise(
    num_pares = n(),
    media = mean(similaridade),
    mediana = median(similaridade),
    min = min(similaridade),
    max = max(similaridade),
    .groups = "drop" # Remove o agrupamento após o summarise
  ) %>%
  ungroup() %>%
  rename(Topico = topico_1) %>% # Renomeia para 'Topico' para maior clareza
  arrange(Topico) # Ordena pelo ID do tópico

print("\n--- Resumo de Similaridade de Cosseno (APENAS INTERNA) por Tópico ---")
print(analise_similaridade_resumo_interna)
View(analise_similaridade_resumo_interna)
