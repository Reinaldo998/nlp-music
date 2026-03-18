#===================================================================
# Gerador de Boxplots de Similaridade Sincronizados
#===================================================================
# Versão com Escala Y Automática (Auto-Zoom por Tópico)
#===================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# --- 1. Cálculo das Similaridades de Cosseno ---
cat("Iniciando cálculo de similaridade de cosseno para os tópicos selecionados...\n")

lista_similaridades <- list()

# SINCRONIZAÇÃO: Usamos os mesmos Top 10 tópicos do Mapa de Calor
# Certifique-se de que 'top_10_topicos_por_musicas' está na memória
topicos_estudo <- as.character(top_10_topicos_por_musicas) 
codigos_validos_matriz <- row.names(matriz_sim_cosseno_total)

for (topico_i in topicos_estudo) {
  # Filtra IDs das músicas do tópico i que existem na matriz de similaridade
  codigos_topico_i <- data_dp %>%
    filter(Grupos_MM_DP == topico_i) %>%
    pull(ID) %>%
    as.character()
  codigos_topico_i <- codigos_topico_i[codigos_topico_i %in% codigos_validos_matriz]
  
  for (topico_j in topicos_estudo) {
    codigos_topico_j <- data_dp %>%
      filter(Grupos_MM_DP == topico_j) %>%
      pull(ID) %>%
      as.character()
    codigos_topico_j <- codigos_topico_j[codigos_topico_j %in% codigos_validos_matriz]
    
    if (length(codigos_topico_i) > 0 & length(codigos_topico_j) > 0) {
      sub_matriz <- matriz_sim_cosseno_total[codigos_topico_i, codigos_topico_j, drop = FALSE]
      
      if (topico_i == topico_j) {
        # Similaridade interna: triangular superior para evitar pares duplicados
        sim_scores <- sub_matriz[upper.tri(sub_matriz)]
      } else {
        # Similaridade cruzada: todos os pares entre tópicos diferentes
        sim_scores <- as.vector(sub_matriz)
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

df_similaridades_top10 <- bind_rows(lista_similaridades)

# --- 2. Paleta de Cores e Ordenação Numérica ---
topicos_ordenados_char <- as.character(sort(as.numeric(topicos_estudo)))
paleta_cores <- scales::hue_pal()(length(topicos_ordenados_char))
names(paleta_cores) <- topicos_ordenados_char

# --- 3. Loop de Salvamento Individual com Escala Automática ---
cat("\nIniciando salvamento individual dos PDFs com auto-zoom...\n")

for (topico_alvo in topicos_estudo) {
  
  # Preparação dos dados para o plot do tópico atual
  df_plot <- df_similaridades_top10 %>%
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
                             y = similaridade, 
                             fill = topico_cor)) +
      geom_boxplot(alpha = 0.7, outlier.size = 0.5, lwd = 0.6) +
      
      # Escala Y Automática (Auto-scaling) para maximizar o detalhe das caixas
      # Removida qualquer restrição manual de limites
      
      scale_x_discrete(labels = setNames(df_plot$rotulo, df_plot$topico_2)) +
      scale_fill_manual(values = paleta_cores) +
      labs(
        title = paste0("Similaridade do tópico ", topico_alvo, " com outros tópicos"),
        #subtitle = "Escala automática para máxima visibilidade das variações",
        x = "Tópico comparado",
        y = "Similaridade de Cosseno"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "none", # Remove a legenda lateral
        panel.grid.minor = element_blank(),
        # Margens padronizadas em 5pt
        plot.margin = margin(t = 15, r = 10, b = 15, l = 10) 
      )
    
    # Nome do arquivo usando o prefixo de similaridade
    nome_arquivo <- paste0("boxplot_sim_topico_", topico_alvo, ".pdf")
    
    # Salvamento com proporção 8x5 (Paisagem)
    ggsave(nome_arquivo, plot = p, width = 8, height = 5, device = "pdf")
    
    cat("Arquivo de similaridade salvo:", nome_arquivo, "\n")
  }
}

cat("\nProcesso concluído. Os boxplots de similaridade foram gerados com auto-zoom.\n")