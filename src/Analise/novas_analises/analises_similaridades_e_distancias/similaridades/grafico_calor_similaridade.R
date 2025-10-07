#===================================================================
# Carregamento de Pacotes
#===================================================================
library(dplyr)
library(lsa)
library(tidyr)
library(ggplot2)
library(purrr)
library(scales)
library(RColorBrewer)
library(stringr)
library(tibble)

# --- Supondo que os dataframes e matrizes necessários já estão na memória ---
# - matriz_sim_cosseno_total (a matriz completa de similaridade entre todas as músicas)
# - data_dp (dataframe com ID, Grupos_MM_DP, etc. - sua base principal com os grupos)
# - resumo_topicos_estudo (o resumo de tópicos)

# --- 1. Recriar um dataframe longo com TODAS as similaridades de todos os pares de tópicos ---
# Esta lógica garante que todos os pares de similaridade cruzada existam.
cat("Calculando todas as similaridades de cosseno entre pares de tópicos...\n")

lista_similaridades <- list()
topicos_existentes <- sort(unique(data_dp$Grupos_MM_DP))

# --- Identificar IDs Válidos da matriz de similaridade ---
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
      sub_matriz_sim <- matriz_sim_cosseno_total[codigos_topico_i, codigos_topico_j, drop = FALSE]
      
      if (topico_i == topico_j) {
        sim_scores <- sub_matriz_sim[upper.tri(sub_matriz_sim)]
      } else {
        sim_scores <- as.vector(sub_matriz_sim)
      }
      
      if (length(sim_scores) > 0) {
        lista_similaridades[[paste(topico_i, topico_j, sep = "_")]] <- tibble(
          topico_1 = topico_i,
          topico_2 = topico_j,
          similaridade = sim_scores
        )
      }
    }
  }
}

df_similaridades_completo <- bind_rows(lista_similaridades)

if (nrow(df_similaridades_completo) == 0) {
  stop("Nenhum score de similaridade foi calculado. Verifique a matriz de similaridade total e a atribuição de tópicos.")
}
cat("Dataframe de similaridades completo criado com sucesso.\n")

#===================================================================
# --- 2. Criar Matrizes de Média e Mediana de Similaridade ---
#===================================================================
cat("\nCriando as matrizes de similaridade média e mediana...\n")

# Calcula a média e a mediana da similaridade para cada par de tópicos
df_sim_resumo <- df_similaridades_completo %>%
  group_by(topico_1, topico_2) %>%
  summarise(
    media_sim = mean(similaridade, na.rm = TRUE),
    mediana_sim = median(similaridade, na.rm = TRUE),
    .groups = "drop"
  )

# Identifica os 10 tópicos com o maior número de músicas
top_10_topicos_por_musicas <- resumo_topicos_estudo %>%
  arrange(desc(numero_de_musicas)) %>%
  head(10) %>%
  pull(Grupos_MM_DP)

# Filtra o resumo para incluir apenas os top 10 tópicos
df_sim_resumo_top10 <- df_sim_resumo %>%
  filter(topico_1 %in% top_10_topicos_por_musicas & topico_2 %in% top_10_topicos_por_musicas)

# Pivota o dataframe para criar a matriz de similaridade média
matriz_medias_sim_cosseno <- df_sim_resumo_top10 %>%
  select(topico_1, topico_2, media_sim) %>%
  pivot_wider(names_from = topico_2, values_from = media_sim) %>%
  column_to_rownames(var = "topico_1") %>%
  as.matrix()

# Pivota o dataframe para criar a matriz de similaridade mediana
matriz_medianas_sim_cosseno <- df_sim_resumo_top10 %>%
  select(topico_1, topico_2, mediana_sim) %>%
  pivot_wider(names_from = topico_2, values_from = mediana_sim) %>%
  column_to_rownames(var = "topico_1") %>%
  as.matrix()

#===================================================================
# --- 3. Gerar os Mapas de Calor para Média e Mediana ---
#===================================================================
# --- Preparar os dados para o mapa de calor de MEDIAS ---

df_medias_sim_longo <- matriz_medias_sim_cosseno %>%
  as.data.frame() %>%
  rownames_to_column(var = "topico_1") %>%
  pivot_longer(
    cols = -topico_1,
    names_to = "topico_2",
    values_to = "media"
  )

topicos_ordenados <- sort(as.numeric(top_10_topicos_por_musicas))
topicos_ordenados_char <- as.character(topicos_ordenados)

df_medias_sim_longo_triangular <- df_medias_sim_longo %>%
  # Filtra para a matriz triangular SUPERIOR
  filter(as.numeric(topico_1) <= as.numeric(topico_2)) %>%
  mutate(
    # x: colunas (esquerda -> direita)
    topico_2 = factor(topico_2, levels = topicos_ordenados_char), 
    # y: linhas (topo -> base)
    topico_1 = factor(topico_1, levels = rev(topicos_ordenados_char))
  )

# --- Gerar o mapa de calor para as MEDIAS ---
cat("\nGerando Mapa de Calor para a Média das Similaridades de Cosseno...\n")

p_medias <- ggplot(df_medias_sim_longo_triangular,
                   aes(x = topico_2, y = topico_1, fill = media)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = str_pad(round(media, 2), 4, "right", "0")), 
            color = "#000000", size = 3, fontface = "bold") +
  scale_fill_gradientn(
    colors = rev(brewer.pal(9, "RdYlBu")),
    name = "Média Similaridade",
    limits = c(0, 1)
  ) +
  labs(
    title = "Mapa de Calor - Média da Similaridade de Cosseno entre Tópicos",
    x = "Tópico",
    y = "Tópico"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#424242"),
    axis.text.y = element_text(size = 10, color = "#424242"),
    axis.title = element_blank(),
    legend.position = "right"
  )

print(p_medias)

# --- Preparar os dados para o mapa de calor de MEDIANAS ---
df_medianas_sim_longo <- matriz_medianas_sim_cosseno %>%
  as.data.frame() %>%
  rownames_to_column(var = "topico_1") %>%
  pivot_longer(
    cols = -topico_1,
    names_to = "topico_2",
    values_to = "mediana"
  )

df_medianas_sim_longo_triangular <- df_medianas_sim_longo %>%
  # Filtra para a matriz triangular SUPERIOR
  filter(as.numeric(topico_1) <= as.numeric(topico_2)) %>%
  mutate(
    # x: colunas (esquerda -> direita)
    topico_2 = factor(topico_2, levels = topicos_ordenados_char),
    # y: linhas (topo -> base)
    topico_1 = factor(topico_1, levels = rev(topicos_ordenados_char))
  )

# --- Gerar o mapa de calor para as MEDIANAS ---
cat("\nGerando Mapa de Calor para a Mediana das Similaridades de Cosseno...\n")

p_medianas <- ggplot(df_medianas_sim_longo_triangular,
                     aes(x = topico_2, y = topico_1, fill = mediana)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = str_pad(round(mediana, 2), 4, "right", "0")), 
            color = "#000000", size = 3, fontface = "bold") +
  scale_fill_gradientn(
    colors = rev(brewer.pal(9, "RdYlBu")),
    name = "Mediana Similaridade",
    limits = c(0, 1)
  ) +
  labs(
    title = "Mapa de Calor - Mediana da Similaridade de Cosseno entre Tópicos",
    x = "Tópico",
    y = "Tópico"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#424242"),
    axis.text.y = element_text(size = 10, color = "#424242"),
    axis.title = element_blank(),
    legend.position = "right"
  )

print(p_medianas)

cat("\nTodos os plots foram gerados com as matrizes triangulares superiores corretas.\n")
