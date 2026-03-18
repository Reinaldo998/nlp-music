#===================================================================
# Gerador de Boxplots JSD Sincronizados com o Mapa de Calor
#===================================================================
# Versão com Escala Y Automática (Auto-Zoom por Tópico)
#===================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# --- 1. Cálculo das Distâncias JSD ---
cat("A iniciar o cálculo de JSD para os tópicos selecionados...\n")

lista_distancias_jsd <- list()

# SINCRONIZAÇÃO: Usamos obrigatoriamente a lista de tópicos do Mapa de Calor
# Certifique-se de que a variável 'top_10_topicos_por_musicas' existe na memória
topicos_estudo <- as.character(top_10_topicos_por_musicas) 
codigos_validos_matriz_jsd <- row.names(matriz_dist_jsd)

for (topico_i in topicos_estudo) {
  # Filtra os IDs das músicas do tópico i que existem na matriz JSD
  codigos_topico_i <- data_dp %>%
    filter(Grupos_MM_DP == topico_i) %>%
    pull(ID) %>%
    as.character()
  codigos_topico_i <- codigos_topico_i[codigos_topico_i %in% codigos_validos_matriz_jsd]
  
  for (topico_j in topicos_estudo) {
    codigos_topico_j <- data_dp %>%
      filter(Grupos_MM_DP == topico_j) %>%
      pull(ID) %>%
      as.character()
    codigos_topico_j <- codigos_topico_j[codigos_topico_j %in% codigos_validos_matriz_jsd]
    
    if (length(codigos_topico_i) > 0 & length(codigos_topico_j) > 0) {
      sub_matriz <- matriz_dist_jsd[codigos_topico_i, codigos_topico_j, drop = FALSE]
      
      if (topico_i == topico_j) {
        # Distância interna: triangular superior para evitar duplicados
        dist_scores <- sub_matriz[upper.tri(sub_matriz)]
      } else {
        # Distância cruzada: todos os pares entre os dois tópicos
        dist_scores <- as.vector(sub_matriz)
      }
      
      if (length(dist_scores) > 0) {
        lista_distancias_jsd[[paste(topico_i, topico_j, sep = "_")]] <- tibble(
          topico_1 = topico_i,
          topico_2 = topico_j,
          distancia = dist_scores
        )
      }
    }
  }
}

df_distancias_jsd_top10 <- bind_rows(lista_distancias_jsd)

# --- 2. Paleta de Cores e Ordenação Numérica ---
topicos_ordenados_char <- as.character(sort(as.numeric(topicos_estudo)))
paleta_cores <- scales::hue_pal()(length(topicos_ordenados_char))
names(paleta_cores) <- topicos_ordenados_char

# --- 3. Ciclo de Salvamento Individual com Escala Automática ---
cat("\nA iniciar o salvamento individual dos ficheiros PDF...\n")

for (topico_alvo in topicos_estudo) {
  
  # Preparação dos dados para o gráfico do tópico atual
  df_plot <- df_distancias_jsd_top10 %>%
    filter(topico_1 == topico_alvo) %>%
    mutate(
      rotulo = case_when(
        topico_1 == topico_2 ~ paste0("Tópico ", topico_alvo, " (Interno)"),
        TRUE ~ paste0("Tópico ", topico_2)
      ),
      topico_cor = factor(as.character(topico_2), levels = topicos_ordenados_char)
    )
  
  if (nrow(df_plot) > 0) {
    p <- ggplot(df_plot, aes(x = factor(topico_2, levels = topicos_ordenados_char), 
                             y = distancia, 
                             fill = topico_cor)) +
      geom_boxplot(alpha = 0.7, outlier.size = 0.5, lwd = 0.6) +
      
      # Deixamos o eixo Y em modo automático para maximizar a visibilidade (Auto-zoom)
      
      scale_x_discrete(labels = setNames(df_plot$rotulo, df_plot$topico_2)) +
      scale_fill_manual(values = paleta_cores) +
      labs(
        title = paste0("Distância JSD do tópico ", topico_principal, " com outros tópicos"),
        #subtitle = "Escala automática para maximizar o detalhe visual dos boxplots",
        x = "Tópico comparado",
        y = "Distância JSD"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none", # Remove a legenda lateral conforme solicitado
        panel.grid.minor = element_blank(),
        # Margens definidas como 5pt em todos os lados
        plot.margin = margin(t = 15, r = 10, b = 15, l = 10) 
      )
    
    # Nome do ficheiro dinâmico
    nome_ficheiro <- paste0("boxplot_jsd_topico_", topico_alvo, ".pdf")
    
    # Salvamento com proporção 8x5 (Largo)
    ggsave(nome_ficheiro, plot = p, width = 9, height = 6, device = "pdf")
    
    cat("Ficheiro guardado:", nome_ficheiro, "\n")
  }
}

cat("\nProcesso concluído com sucesso. Verifique os PDFs na sua pasta de trabalho.\n")