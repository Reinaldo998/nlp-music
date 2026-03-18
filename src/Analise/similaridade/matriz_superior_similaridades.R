#===================================================================
# Gráfico de Heatmap de Similaridade - Padrão Triangular Superior
#===================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(stringr)
library(viridis)
library(tibble)

# --- 1. Calcular scores de similaridade e organizar em formato longo ---
cat("Calculando similaridades de cosseno entre pares de tópicos...\n")

lista_similaridades <- list()
topicos_existentes <- sort(unique(data_dp$Grupos_MM_DP))
codigos_validos_matriz <- row.names(matriz_sim_cosseno_total)

for (topico_i in topicos_existentes) {
  codigos_topico_i <- data_dp %>%
    filter(Grupos_MM_DP == topico_i) %>%
    pull(ID) %>%
    as.character()
  
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

# --- 2. Criar Resumo e Filtrar Top 10 Tópicos ---
cat("\nCriando as matrizes de similaridade média e mediana...\n")

df_sim_resumo_top10 <- df_similaridades_completo %>%
  group_by(topico_1, topico_2) %>%
  summarise(
    media_sim = mean(similaridade, na.rm = TRUE),
    mediana_sim = median(similaridade, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Filtra apenas para os 10 tópicos mais populosos definidos anteriormente
  filter(topico_1 %in% top_10_topicos_por_musicas & topico_2 %in% top_10_topicos_por_musicas)

topicos_ordenados <- sort(as.numeric(top_10_topicos_por_musicas))
topicos_ordenados_char <- as.character(topicos_ordenados)

#===================================================================
# --- 3. Gerar os Mapas de Calor (PADRÃO TRIANGULAR SUPERIOR) ---
#===================================================================

# --- A. Preparar dados de MÉDIAS ---
df_medias_triangular <- df_sim_resumo_top10 %>%
  select(topico_1, topico_2, media = media_sim) %>%
  # FILTRO ESSENCIAL: Matriz Triangular Superior
  filter(as.numeric(topico_1) <= as.numeric(topico_2)) %>%
  mutate(
    topico_2 = factor(as.character(topico_2), levels = topicos_ordenados_char), 
    topico_1 = factor(as.character(topico_1), levels = rev(topicos_ordenados_char))
  )

# Gerar Plot de Médias
p_medias <- ggplot(df_medias_triangular, aes(x = topico_2, y = topico_1, fill = media)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", media)), 
            color = "white", size = 2.8, fontface = "bold") +
  scale_fill_gradientn(
    colors = viridis::magma(256),
    name = "Similaridade",
    limits = c(0, 1),
    # Ajuste o values conforme o intervalo real para ter contraste (ex: 0.01 a 0.10)
    values = scales::rescale(c(0, 0.01, 0.10, 1)),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  labs(
    title = "Média da similaridade de cosseno entre tópicos",
    subtitle = "Representação triangular superior",
    x = "Tópico", y = "Tópico"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "#424242"),
    axis.text.y = element_text(size = 9, color = "#424242"),
    axis.title = element_blank(),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.margin = margin(t = 5, r = 2, b = 5, l = 2)
  )

print(p_medias)


# --- B. Preparar dados de MEDIANAS ---
df_medianas_triangular <- df_sim_resumo_top10 %>%
  select(topico_1, topico_2, mediana = mediana_sim) %>%
  filter(as.numeric(topico_1) <= as.numeric(topico_2)) %>%
  mutate(
    topico_2 = factor(as.character(topico_2), levels = topicos_ordenados_char),
    topico_1 = factor(as.character(topico_1), levels = rev(topicos_ordenados_char))
  )

# Gerar Plot de Medianas
p_medianas <- ggplot(df_medianas_triangular, aes(x = topico_2, y = topico_1, fill = mediana)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", mediana)), 
            color = "white", size = 2.8, fontface = "bold") +
  scale_fill_gradientn(
    colors = viridis::magma(256),
    name = "Mediana",
    limits = c(0, 1),
    values = scales::rescale(c(0, 0.01, 0.10, 1)),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  labs(
    title = "Mediana da similaridade de cosseno entre tópicos",
    subtitle = "Representação triangular superior",
    x = "Tópico", y = "Tópico"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "#424242"),
    axis.text.y = element_text(size = 9, color = "#424242"),
    axis.title = element_blank(),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.margin = margin(t = 5, r = 2, b = 5, l = 2)
  )

# --- 4. Exibição e Salvamento ---

print(p_medianas)

# Salvamento em PDF
pdf("sim_cosseno_triangular_superior.pdf", width = 8, height = 5)
print(p_medias)
print(p_medianas)
dev.off()

cat("\nArquivos salvos com sucesso no formato triangular superior.\n")