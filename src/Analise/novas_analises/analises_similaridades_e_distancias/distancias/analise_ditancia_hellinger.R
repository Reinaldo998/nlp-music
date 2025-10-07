# --- Carregar os pacotes necessários ---
library(dplyr)
library(tidyr)
library(ggplot2)
library(philentropy) # Para garantir que as funções de distância são conhecidas

# --- Supondo que 'matriz_dist_hellinger' e 'data_com_topicos_atual' já estão na memória ---
# A matriz de distância de Hellinger foi calculada previamente e está na escala [0, 1].

cat("Iniciando a análise de coerência por tópico com a Distância de Hellinger...\n")

# --- 1. Calcular todos os scores de DISTÂNCIA e armazenar em formato longo ---
# (MÉTODO OTIMIZADO)
resultados_distancia_hellinger_longo <- list()
topicos_existentes <- sort(unique(data_com_topicos_atual$topico_lda))

for (topico_id in topicos_existentes) {
  # Filtra o dataframe para o tópico atual
  musicas_do_topico <- data_com_topicos_atual %>%
    filter(topico_lda == topico_id)
  
  # Apenas continua se houver mais de uma música no tópico
  if (nrow(musicas_do_topico) > 1) {
    # EXTRAÇÃO OTIMIZADA: Pega a sub-matriz de distância da matriz TOTAL
    # usando os Códigos das músicas como index
    codigos_do_topico <- as.character(musicas_do_topico$ID)
    
    # Certifique-se de que os códigos existem na matriz de distância
    codigos_validos_no_topico <- codigos_do_topico[codigos_do_topico %in% row.names(matriz_dist_hellinger)]
    
    if (length(codigos_validos_no_topico) > 1) {
      matriz_do_topico_hellinger <- matriz_dist_hellinger[codigos_validos_no_topico, codigos_validos_no_topico]
      
      # Extrai a triangular superior da matriz (sem a diagonal) para pegar os pares únicos
      dist_pares_hellinger <- matriz_do_topico_hellinger[upper.tri(matriz_do_topico_hellinger)]
      
      # Armazena os resultados na lista
      resultados_distancia_hellinger_longo[[as.character(topico_id)]] <- tibble(
        topico = as.integer(topico_id),
        distancia = dist_pares_hellinger
      )
    }
  }
}

# Combina todos os resultados de distância em um único dataframe longo
df_distancias_hellinger_longo <- bind_rows(resultados_distancia_hellinger_longo)

if (nrow(df_distancias_hellinger_longo) == 0) {
  stop("Nenhum score de distância de Hellinger foi calculado. Verifique se os tópicos têm mais de uma música e IDs válidos.")
}
cat("Extração de distâncias de Hellinger concluída.\n")


# --- 2. Calcular Métrica de Resumo (Média, Mediana, Mínimo, Máximo) ---
analise_distancia_hellinger_resumo <- df_distancias_hellinger_longo %>%
  group_by(topico) %>%
  summarise(
    num_pares = n(),
    media = mean(distancia),
    mediana = median(distancia),
    min = min(distancia),
    max = max(distancia)
  ) %>%
  ungroup() %>%
  arrange(topico)

print("\n--- Resumo de Distância de Hellinger por Tópico ---\n")
View(analise_distancia_hellinger_resumo)
print(analise_distancia_hellinger_resumo)


# --- 3. Gerar Boxplot da Distância de Hellinger por Tópico ---
cat("\n--- Gerando Boxplot da Distância de Hellinger por Tópico ---\n")
ggplot(df_distancias_hellinger_longo, aes(x = factor(topico), y = distancia, fill = factor(topico))) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribuição da Distância de Hellinger entre Músicas por Tópico",
    x = "Tópico",
    y = "Distância de Hellinger (0 = idêntico, 1 = completamente diferente)",
    fill = "Tópico"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Opcional: Salvar o boxplot como arquivo de imagem
# ggsave("boxplot_distancia_hellinger_topicos.png", width = 10, height = 6)
