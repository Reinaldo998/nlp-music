library(dplyr)
library(lsa)
library(tidyr)
library(ggplot2)
library(purrr)

# --- Supondo que os dataframes e matrizes necessários já estão na memória ---
# - matriz_sim_cosseno_total (a matriz completa de similaridade entre todas as músicas)
# - data_com_topicos_atual (dataframe com ID, topico_lda, etc.)

# --- 1. Recriar um dataframe longo com TODAS as similaridades de todos os pares de tópicos ---
# Esta lógica garante que todos os pares de similaridade cruzada existam.
cat("Calculando todas as similaridades de cosseno entre pares de tópicos...\n")

lista_similaridades <- list()
topicos_existentes <- sort(unique(data_com_topicos_atual$topico_lda))

for (topico_i in topicos_existentes) {
  # Apenas pegar os IDs das músicas do tópico i
  codigos_topico_i <- data_com_topicos_atual %>%
    filter(topico_lda == topico_i) %>%
    pull(ID)
  
  for (topico_j in topicos_existentes) {
    # Apenas pegar os IDs das músicas do tópico j
    codigos_topico_j <- data_com_topicos_atual %>%
      filter(topico_lda == topico_j) %>%
      pull(ID)
    
    # Continuar se houver músicas em ambos os tópicos para comparação
    if (length(codigos_topico_i) > 0 & length(codigos_topico_j) > 0) {
      sub_matriz_sim <- matriz_sim_cosseno_total[as.character(codigos_topico_i), as.character(codigos_topico_j)]
      
      # Extrair os scores de similaridade para o formato longo
      # Se i == j, extrai a triangular superior (similaridade interna)
      if (topico_i == topico_j) {
        sim_scores <- sub_matriz_sim[upper.tri(sub_matriz_sim)]
      } else {
        # Se i != j, extrai todos os valores (similaridade externa)
        sim_scores <- as.vector(sub_matriz_sim)
      }
      
      # Adicionar os scores à lista
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


# --- 2. Gerar e exibir um plot de boxplots para CADA tópico principal ---
cat("\nGerando plots de similaridade por tópico...\n")

# Definir a Paleta de Cores e a Ordem dos Tópicos Globalmente
topicos_existentes <- sort(unique(df_similaridades_completo$topico_1))

paleta_de_cores_global <- scales::hue_pal()(length(topicos_existentes))
names(paleta_de_cores_global) <- as.character(topicos_existentes)

for (topico_principal in topicos_existentes) {
  df_plot <- df_similaridades_completo %>%
    filter(topico_1 == topico_principal) %>%
    mutate(
      topico_comparado_id = topico_2,
      rotulo = case_when(
        topico_1 == topico_2 ~ paste0("Tópico ", topico_principal, " (Interno)"),
        TRUE ~ paste0("Tópico ", topico_2)
      ),
      topico_cor = factor(as.character(topico_2), levels = as.character(topicos_existentes))
    )
  
  if (nrow(df_plot) > 0) {
    p <- ggplot(df_plot, aes(x = factor(topico_comparado_id),
                             y = similaridade, 
                             fill = topico_cor)) +
      geom_boxplot(alpha = 0.7) +
      scale_x_discrete(labels = setNames(df_plot$rotulo, df_plot$topico_comparado_id)) +
      labs(
        title = paste0("Similaridade do Tópico ", topico_principal, " com Outros Tópicos"),
        x = "Tópico Comparado",
        y = "Similaridade de Cosseno",
        fill = "Tópico"
      ) +
      theme_minimal() +
      scale_fill_manual(values = paleta_de_cores_global,
                        breaks = names(paleta_de_cores_global)) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    print(p)
  }
}
cat("\nTodos os plots foram gerados com cores, posições e rótulos consistentes, e a legenda em ordem numérica.\n")