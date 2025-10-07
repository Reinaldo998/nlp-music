library(dplyr)
library(lsa)
library(tidyr)
library(ggplot2)

# --- Supondo que a 'matriz_sim_cosseno_total' e 'data_com_topicos_lda' já estão na memória ---
# A matriz de similaridade total foi calculada uma única vez antes do loop.

# --- 1. Calcular todos os scores de similaridade e armazenar em formato longo (MÉTODO OTIMIZADO) ---
cat("Extraindo scores de similaridade da matriz completa para cada tópico...\n")

resultados_similaridade_longo <- list()
topicos_existentes <- sort(unique(data_com_topicos_atual$topico_lda))

for (topico_id in topicos_existentes) {
  # Filtra o dataframe para o tópico atual
  musicas_do_topico <- data_com_topicos_atual %>%
    filter(topico_lda == topico_id)

  
  # Apenas continua se houver mais de uma música no tópico
  if (nrow(musicas_do_topico) > 1) {
    # EXTRAÇÃO OTIMIZADA: Pega a sub-matriz de similaridade da matriz TOTAL
    # usando os Códigos das músicas como index
    codigos_do_topico <- as.character(musicas_do_topico$ID)
    matriz_do_topico <- matriz_sim_cosseno_total[codigos_do_topico, codigos_do_topico]
    
    # Extrai a triangular superior da matriz (sem a diagonal) para pegar os pares únicos
    sim_pares <- matriz_do_topico[upper.tri(matriz_do_topico)]
    
    # Armazena os resultados na lista
    resultados_similaridade_longo[[as.character(topico_id)]] <- tibble(
      topico = as.integer(topico_id),
      similaridade = sim_pares
    )
  }
}

View(as.matrix(sim_pares))

max(as.matrix(sim_pares))
# Combina todos os resultados de similaridade em um único dataframe longo
df_similaridades_longo <- bind_rows(resultados_similaridade_longo)

if (nrow(df_similaridades_longo) == 0) {
  stop("Nenhum score de similaridade foi calculado. Verifique se os tópicos têm mais de uma música.")
}
cat("Extração de similaridades concluída.\n")


# --- 2. Calcular Métrica de Resumo (Média, Mediana, Mínimo, Máximo) ---
analise_similaridade_resumo <- df_similaridades_longo %>%
  group_by(topico) %>%
  summarise(
    num_pares = n(),
    media = mean(similaridade),
    mediana = median(similaridade),
    min = min(similaridade),
    max = max(similaridade)
  ) %>%
  ungroup() %>%
  arrange(topico)

print("\n--- Resumo de Similaridade por Tópico ---\n")
View(analise_similaridade_resumo)
print(analise_similaridade_resumo)


# --- 3. Gerar Boxplot da Similaridade por Tópico ---
cat("\n--- Gerando Boxplot da Similaridade por Tópico ---\n")
ggplot(df_similaridades_longo, aes(x = factor(topico), y = similaridade, fill = factor(topico))) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribuição da Similaridade de Cosseno entre Músicas por Tópico",
    x = "Tópico",
    y = "Similaridade de Cosseno",
    fill = "Tópico"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Opcional: Salvar o boxplot como arquivo de imagem
# ggsave("boxplot_similaridade_topicos.png", width = 10, height = 6)

