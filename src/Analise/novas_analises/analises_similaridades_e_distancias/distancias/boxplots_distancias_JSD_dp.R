#===================================================================
# Carregamento de Pacotes
#===================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(scales) # Para scales::hue_pal()
library(philentropy) # Para JSD, embora a matriz já seja assumida

#
#===================================================================
# Assumindo que os dataframes e matrizes necessários já estão na memória:
# - matriz_dist_jsd (a matriz completa de DISTÂNCIA JSD entre todas as músicas)
# - data_dp (dataframe com ID, Grupos_MM_DP, etc. - sua base principal com os grupos)
# - resumo_topicos_estudo (o resumo de tópicos)
# - top_10_topicos_por_musicas (os IDs dos 10 tópicos mais frequentes)
#===================================================================

cat("Iniciando a análise de Distância de Jensen-Shannon (JSD) entre pares de tópicos...\n")

# --- 1. Calcular todos os scores de DISTÂNCIA JSD e armazenar em formato longo ---
# Esta lógica garante que todos os pares de distância cruzada existam.
lista_distancias_jsd <- list()
topicos_existentes <- sort(unique(data_dp$Grupos_MM_DP))

# Identificar IDs Válidos da matriz de distância
codigos_validos_matriz_jsd <- row.names(matriz_dist_jsd)


for (topico_i in topicos_existentes) {
  # Apenas pegar os IDs das músicas do tópico i que são Códigos válidos na matriz JSD
  codigos_topico_i <- data_dp %>%
    filter(Grupos_MM_DP == topico_i) %>%
    pull(ID) %>%
    as.character()
  
  # Filtrar para garantir que o código realmente existe na matriz de distância
  codigos_topico_i <- codigos_topico_i[codigos_topico_i %in% codigos_validos_matriz_jsd]
  
  for (topico_j in topicos_existentes) {
    codigos_topico_j <- data_dp %>%
      filter(Grupos_MM_DP == topico_j) %>%
      pull(ID) %>%
      as.character()
    
    codigos_topico_j <- codigos_topico_j[codigos_topico_j %in% codigos_validos_matriz_jsd]
    
    if (length(codigos_topico_i) > 0 & length(codigos_topico_j) > 0) {
      # Extrai a sub-matriz de distância JSD
      sub_matriz_dist_jsd <- matriz_dist_jsd[codigos_topico_i, codigos_topico_j, drop = FALSE]
      
      if (topico_i == topico_j) {
        # Para o mesmo tópico, pegamos a triangular superior (distância interna)
        # E agora, também os IDs das músicas que formam esses pares
        
        # Obtém os índices da triangular superior
        indices_upper_tri <- which(upper.tri(sub_matriz_dist_jsd), arr.ind = TRUE)
        
        # Extrai os scores de distância
        dist_scores <- sub_matriz_dist_jsd[upper.tri(sub_matriz_dist_jsd)]
        
        # Extrai os IDs das músicas para cada par
        id_musica_1 <- row.names(sub_matriz_dist_jsd)[indices_upper_tri[,1]]
        id_musica_2 <- colnames(sub_matriz_dist_jsd)[indices_upper_tri[,2]]
        
      } else {
        # Para tópicos diferentes, pegamos todas as distâncias (distância cruzada)
        # E também os IDs das músicas que formam esses pares
        
        dist_scores <- as.vector(sub_matriz_dist_jsd)
        
        # Cria todas as combinações de IDs para os pares cruzados
        id_musica_1 <- rep(row.names(sub_matriz_dist_jsd), times = ncol(sub_matriz_dist_jsd))
        id_musica_2 <- rep(colnames(sub_matriz_dist_jsd), each = nrow(sub_matriz_dist_jsd))
      }
      
      lista_distancias_jsd[[paste(topico_i, topico_j, sep = "_")]] <- tibble(
        topico_1 = topico_i,
        topico_2 = topico_j,
        ID_musica_1 = id_musica_1, # Nova coluna
        ID_musica_2 = id_musica_2, # Nova coluna
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


# --- 2. Filtrar para os TOP 10 Tópicos e Definir Paleta de Cores ---
# top_10_topicos_por_musicas já deve estar carregado
# top_10_topicos_por_musicas <- resumo_topicos_estudo %>%
#   arrange(desc(numero_de_musicas)) %>%
#   head(10) %>%
#   pull(Grupos_MM_DP)

df_distancias_jsd_top10 <- df_distancias_jsd_completo %>%
  filter(topico_1 %in% top_10_topicos_por_musicas & topico_2 %in% top_10_topicos_por_musicas)

# --- CORREÇÃO AQUI: Criar uma lista ordenada NUMERICAMENTE dos tópicos para a legenda ---
# Garante a ordem numérica correta, não lexicográfica
topicos_ordenados_numeric_para_legenda <- sort(as.numeric(top_10_topicos_por_musicas)) # Ordena numericamente
topicos_ordenados_numeric_para_legenda <- as.character(topicos_ordenados_numeric_para_legenda) # Depois converte para caractere

# Definir a Paleta de Cores e a Ordem dos Tópicos Globalmente (agora para os Top 10)
paleta_de_cores_global <- scales::hue_pal()(length(topicos_ordenados_numeric_para_legenda))
names(paleta_de_cores_global) <- topicos_ordenados_numeric_para_legenda


# --- 3. Gerar e exibir os plots de boxplots para os TOP 10 tópicos ---
cat("\nGerando plots de Distância de Jensen-Shannon por tópico...\n")

for (topico_principal in top_10_topicos_por_musicas) {
  df_plot <- df_distancias_jsd_top10 %>% # Usando o dataframe filtrado para os top 10
    filter(topico_1 == topico_principal) %>%
    mutate(
      topico_comparado_id = topico_2,
      rotulo = case_when(
        topico_1 == topico_2 ~ paste0("Tópico ", topico_principal, " (Interno)"),
        TRUE ~ paste0("Tópico ", topico_2)
      ),
      # --- ALTERADO: Usar topicos_ordenados_numeric_para_legenda para os levels ---
      topico_cor = factor(as.character(topico_2), levels = topicos_ordenados_numeric_para_legenda)
    )
  
  if (nrow(df_plot) > 0) {
    p <- ggplot(df_plot, aes(x = factor(topico_comparado_id),
                             y = distancia, # Eixo Y agora é 'distancia'
                             fill = topico_cor)) +
      geom_boxplot(alpha = 0.7) +
      scale_x_discrete(labels = setNames(df_plot$rotulo, df_plot$topico_comparado_id)) +
      labs(
        title = paste0("Distância JSD do Tópico ", topico_principal, " com Outros Tópicos"),
        x = "Tópico Comparado",
        y = "Distância de Jensen-Shannon (0 = idêntico, 1 = diferente)", # Rótulo do eixo Y atualizado
        fill = "Tópico"
      ) +
      theme_minimal() +
      # --- ALTERADO: Usar topicos_ordenados_numeric_para_legenda para os breaks ---
      scale_fill_manual(values = paleta_de_cores_global,
                        breaks = topicos_ordenados_numeric_para_legenda,
                        limits = topicos_ordenados_numeric_para_legenda) + # Adicionado limits para reforçar a ordem
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    print(p)
  }
}
cat("\nTodos os plots de Distância JSD foram gerados com cores, posições e rótulos consistentes, e a legenda em ordem numérica.\n")
