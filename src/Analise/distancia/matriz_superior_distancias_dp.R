#===================================================================
# Gráfico de Heatmap JSD - Padrão Triangular Superior
#===================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(stringr)
library(viridis)

# --- 1. Preparar os dados para o mapa de calor de MÉDIAS ---

# Definir a ordem numérica correta dos tópicos
topicos_ordenados <- sort(as.numeric(top_10_topicos_por_musicas))
topicos_ordenados_char <- as.character(topicos_ordenados)

df_medias_jsd_longo_triangular <- df_resumo_pares_distancia %>%
  select(topico_1, topico_2, media = media_distancia) %>%
  # Filtra para a matriz triangular SUPERIOR
  filter(as.numeric(topico_1) <= as.numeric(topico_2)) %>%
  mutate(
    # PADRÃO SUPERIOR: 
    # x: topico_2 (colunas - ordem normal)
    topico_2 = factor(as.character(topico_2), levels = topicos_ordenados_char), 
    # y: topico_1 (linhas - ordem invertida para o 1 ficar na base)
    topico_1 = factor(as.character(topico_1), levels = rev(topicos_ordenados_char))
  )

# --- Gerar o mapa de calor para as MÉDIAS ---
cat("\nGerando Mapa de Calor JSD (Superior) para Médias...\n")

p_medias_jsd <- ggplot(df_medias_jsd_longo_triangular,
                       aes(x = topico_2, y = topico_1, fill = media)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", media)), 
            color = "white", size = 3, fontface = "bold") +
  scale_fill_viridis_c(
    option = "magma",
    name = "Média JSD",
    limits = c(0.55,0.7)#c(min_sim, max_sim)
  ) +
  labs(
    title = "Média da distância JSD entre tópicos",
    subtitle = "Representação triangular superior",
    x = "Tópico",
    y = "Tópico"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#424242"),
    axis.text.y = element_text(size = 10, color = "#424242"),
    axis.title = element_blank(),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.margin = margin(t = 5, r = 2, b = 5, l = 2)
  )

print(p_medias_jsd)


# --- 2. Preparar os dados para o mapa de calor de MEDIANAS ---

df_medianas_jsd_longo_triangular <- df_resumo_pares_distancia %>%
  select(topico_1, topico_2, mediana = mediana_distancia) %>%
  # Filtra para a matriz triangular SUPERIOR
  filter(as.numeric(topico_1) <= as.numeric(topico_2)) %>%
  mutate(
    # Mantendo o mesmo mapeamento do Cosseno
    topico_2 = factor(as.character(topico_2), levels = topicos_ordenados_char),
    topico_1 = factor(as.character(topico_1), levels = rev(topicos_ordenados_char))
  )

# --- Gerar o mapa de calor para as MEDIANAS ---
cat("\nGerando Mapa de Calor JSD (Superior) para Medianas...\n")

p_medianas_jsd <- ggplot(df_medianas_jsd_longo_triangular,
                         aes(x = topico_2, y = topico_1, fill = mediana)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", mediana)), 
            color = "white", size = 3, fontface = "bold") +
  scale_fill_viridis_c(
    option = "magma",
    name = "Mediana JSD",
    limits = c(0.55, 0.7)
  ) +
  labs(
    title = "Mediana da distância JSD entre tópicos",
    subtitle = "Representação triangular superior",
    x = "Tópico",
    y = "Tópico"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#424242"),
    axis.text.y = element_text(size = 10, color = "#424242"),
    axis.title = element_blank(),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.margin = margin(t = 5, r = 2, b = 5, l = 2)
  )

print(p_medianas_jsd)

#########################################################################################
