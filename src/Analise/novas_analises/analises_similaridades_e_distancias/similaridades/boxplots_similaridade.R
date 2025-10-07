library(dplyr)
library(lsa)
library(tidyr)
library(ggplot2)
library(purrr)
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
        sim_scores <- sub_matriz_sim[upper.tri(sub_matriz_sim)]
      } else {
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


# --- 2. Gerar e exibir um plot de boxplots para CADA tópico principal ---
cat("\nGerando plots de similaridade por tópico...\n")

# --- NOVO PASSO: FILTRAR df_similaridades_completo APENAS PARA OS TOP 10 TÓPICOS ---
top_10_topicos_por_musicas <- resumo_topicos_estudo %>%
  arrange(desc(num_musicas)) %>%
  head(10) %>%
  pull(Grupos_MM_DP)

df_similaridades_top10 <- df_similaridades_completo %>%
  filter(topico_1 %in% top_10_topicos_por_musicas & topico_2 %in% top_10_topicos_por_musicas)


# --- 3. Definir a Paleta de Cores e a Ordem dos Tópicos Globalmente (agora para os Top 10) ---
paleta_de_cores_global <- scales::hue_pal()(length(top_10_topicos_por_musicas))
names(paleta_de_cores_global) <- as.character(top_10_topicos_por_musicas)


# --- 4. Gerar e exibir os plots de boxplots para os TOP 10 tópicos ---
for (topico_principal in top_10_topicos_por_musicas) {
  df_plot <- df_similaridades_top10 %>% # <--- AGORA USANDO O DATAFRAME FILTRADO
    filter(topico_1 == topico_principal) %>%
    mutate(
      topico_comparado_id = topico_2,
      rotulo = case_when(
        topico_1 == topico_2 ~ paste0("Tópico ", topico_principal, " (Interno)"),
        TRUE ~ paste0("Tópico ", topico_2)
      ),
      topico_cor = factor(as.character(topico_2), levels = as.character(top_10_topicos_por_musicas))
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