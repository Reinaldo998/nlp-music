# --- Carregar pacotes necessários ---
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(scales)
library(philentropy)
library(RColorBrewer)
library(stringr)
library(tibble)
library(lsa)

# --- 1. Calcular todos os scores de DISTÂNCIA JSD e armazenar em formato longo ---
# Esta lógica garante que todos os pares de distância cruzada existam.
cat("Iniciando a análise de Distância de Jensen-Shannon (JSD) entre pares de tópicos...\n")

lista_distancias_jsd <- list()
topicos_existentes <- sort(unique(data_dp$Grupos_MM_DP))
codigos_validos_matriz_jsd <- row.names(matriz_dist_jsd)

for (topico_i in topicos_existentes) {
  codigos_topico_i <- data_dp %>%
    filter(Grupos_MM_DP == topico_i) %>%
    pull(ID) %>%
    as.character()
  
  codigos_topico_i <- codigos_topico_i[codigos_topico_i %in% codigos_validos_matriz_jsd]
  
  for (topico_j in topicos_existentes) {
    codigos_topico_j <- data_dp %>%
      filter(Grupos_MM_DP == topico_j) %>%
      pull(ID) %>%
      as.character()
    
    codigos_topico_j <- codigos_topico_j[codigos_topico_j %in% codigos_validos_matriz_jsd]
    
    if (length(codigos_topico_i) > 0 & length(codigos_topico_j) > 0) {
      sub_matriz_dist_jsd <- matriz_dist_jsd[codigos_topico_i, codigos_topico_j, drop = FALSE]
      
      if (topico_i == topico_j) {
        indices_upper_tri <- which(upper.tri(sub_matriz_dist_jsd), arr.ind = TRUE)
        dist_scores <- sub_matriz_dist_jsd[upper.tri(sub_matriz_dist_jsd)]
        id_musica_1 <- row.names(sub_matriz_dist_jsd)[indices_upper_tri[,1]]
        id_musica_2 <- colnames(sub_matriz_dist_jsd)[indices_upper_tri[,2]]
      } else {
        dist_scores <- as.vector(sub_matriz_dist_jsd)
        id_musica_1 <- rep(row.names(sub_matriz_dist_jsd), times = ncol(sub_matriz_dist_jsd))
        id_musica_2 <- rep(colnames(sub_matriz_dist_jsd), each = nrow(sub_matriz_dist_jsd))
      }
      
      lista_distancias_jsd[[paste(topico_i, topico_j, sep = "_")]] <- tibble(
        topico_1 = topico_i,
        topico_2 = topico_j,
        ID_musica_1 = id_musica_1,
        ID_musica_2 = id_musica_2,
        distancia = dist_scores
      )
    }
  }
}

df_distancias_jsd_completo <- bind_rows(lista_distancias_jsd)

if (nrow(df_distancias_jsd_completo) == 0) {
  stop("Nenhum score de distância JSD foi calculado. Verifique a matriz de distância total e a atribuição de tópicos.")
}
cat("Dataframe de distâncias JSD completo criado com sucesso.\n")

# --- NOVO PASSO: FILTRAR PARA OS TOP 10 TÓPICOS ANTES DE CRIAR AS MATRIZES ---
top_10_topicos_por_musicas <- resumo_topicos_estudo %>%
  arrange(desc(num_musicas)) %>%
  head(10) %>%
  pull(Grupos_MM_DP)

# Filtra o dataframe de distâncias para incluir apenas os top 10 tópicos em ambos os eixos
df_distancias_jsd_top10 <- df_distancias_jsd_completo %>%
  filter(topico_1 %in% top_10_topicos_por_musicas & topico_2 %in% top_10_topicos_por_musicas)


# --- 1. Calcular a média e mediana da distância para cada par de tópicos ---
df_resumo_pares_distancia <- df_distancias_jsd_top10 %>%
  group_by(topico_1, topico_2) %>%
  summarise(
    media_distancia = round(mean(distancia, na.rm = TRUE), 4),
    mediana_distancia = round(median(distancia, na.rm = TRUE), 4),
    .groups = "drop"
  )

cat("\nResumo da média e mediana de distância por par de tópicos concluído.\n")

# --- 2. Criar a matriz de Médias ---
matriz_medias_dist_jsd <- df_resumo_pares_distancia %>%
  select(-mediana_distancia) %>%
  pivot_wider(
    names_from = topico_2,
    values_from = media_distancia,
    names_sort = TRUE
  ) %>%
  column_to_rownames(var = "topico_1") %>%
  as.matrix()

cat("\nMatriz de médias da distância JSD entre tópicos criada.\n")
cat("Dimensões da matriz de médias:", paste(dim(matriz_medias_dist_jsd), collapse = "x"), "\n")
print(matriz_medias_dist_jsd)

# --- 3. Criar a matriz de Medianas ---
matriz_medianas_dist_jsd <- df_resumo_pares_distancia %>%
  select(-media_distancia) %>%
  pivot_wider(
    names_from = topico_2,
    values_from = mediana_distancia,
    names_sort = TRUE
  ) %>%
  column_to_rownames(var = "topico_1") %>%
  as.matrix()

cat("\nMatriz de medianas da distância JSD entre tópicos criada.\n")
cat("Dimensões da matriz de medianas:", paste(dim(matriz_medianas_dist_jsd), collapse = "x"), "\n")
print(matriz_medianas_dist_jsd)


# --- 4. Gerar os Mapas de Calor para Média e Mediana ---
# --- Preparar os dados para o mapa de calor de MEDIAS ---
df_medias_jsd_longo <- matriz_medias_dist_jsd %>%
  as.data.frame() %>%
  rownames_to_column(var = "topico_1") %>%
  pivot_longer(
    cols = -topico_1,
    names_to = "topico_2",
    values_to = "media"
  )

# Filtra para a matriz triangular inferior e garante a ordenação
topicos_ordenados_char <- as.character(sort(as.numeric(top_10_topicos_por_musicas)))

df_medias_jsd_longo_triangular <- df_medias_jsd_longo %>%
  filter(as.numeric(topico_1) <= as.numeric(topico_2)) %>%
  mutate(
    topico_1 = factor(topico_1, levels = topicos_ordenados_char),
    topico_2 = factor(topico_2, levels = rev(topicos_ordenados_char))
  )

# --- Gerar o mapa de calor para as MEDIAS ---
cat("\nGerando Mapa de Calor para a Média das Distâncias JSD...\n")

p_medias <- ggplot(df_medias_jsd_longo_triangular,
                   aes(x = topico_1, y = topico_2, fill = media)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = str_pad(round(media, 2), 4, "right", "0")),
            color = "#000000", size = 3, fontface = "bold") +
  scale_fill_gradientn(
    colors = rev(brewer.pal(9, "RdYlBu")),
    name = "Média Distância JSD",
    limits = c(0, 1)
  ) +
  labs(
    title = "Mapa de Calor - Média da Distância JSD entre Tópicos",
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
df_medianas_jsd_longo <- matriz_medianas_dist_jsd %>%
  as.data.frame() %>%
  rownames_to_column(var = "topico_1") %>%
  pivot_longer(
    cols = -topico_1,
    names_to = "topico_2",
    values_to = "mediana"
  )

df_medianas_jsd_longo_triangular <- df_medianas_jsd_longo %>%
  filter(as.numeric(topico_1) <= as.numeric(topico_2)) %>%
  mutate(
    topico_1 = factor(topico_1, levels = topicos_ordenados_char),
    topico_2 = factor(topico_2, levels = rev(topicos_ordenados_char))
  )

# --- Gerar o mapa de calor para as MEDIANAS ---
cat("\nGerando Mapa de Calor para a Mediana das Distâncias JSD...\n")

p_medianas <- ggplot(df_medianas_jsd_longo_triangular,
                     aes(x = topico_1, y = topico_2, fill = mediana)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = str_pad(round(mediana, 2), 4, "right", "0")),
            color = "#000000", size = 3, fontface = "bold") +
  scale_fill_gradientn(
    colors = rev(brewer.pal(9, "RdYlBu")),
    name = "Mediana Distância JSD",
    limits = c(0, 1)
  ) +
  labs(
    title = "Mapa de Calor - Mediana da Distância JSD entre Tópicos",
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
