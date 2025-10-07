# --- Carregar pacotes necessários ---
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# --- Assumimos que 'df_distancias_jsd_completo' e 'resumo_topicos_estudo' já estão na memória ---

# --- 1. Identificar os 10 tópicos com mais músicas ---
top_10_topicos_por_musicas <- resumo_topicos_estudo %>%
  arrange(desc(num_musicas)) %>%
  head(10) %>%
  pull(Grupos_MM_DP)

# --- 2. Preparar os dados para o boxplot ---
# Filtra o dataframe para incluir APENAS similaridades internas e dos TOP 10 tópicos
df_distancias_internas_top10 <- df_distancias_jsd_completo %>%
  filter(topico_1 %in% top_10_topicos_por_musicas, topico_2 %in% top_10_topicos_por_musicas) %>%
  filter(topico_1 == topico_2) %>% # Filtra para incluir apenas as distâncias internas
  rename(topico = topico_1) # Renomeia a coluna para facilitar o plot


# --- 3. Gerar Boxplot da Similaridade Interna por Tópico ---
cat("\nGerando Boxplot da Coerência Interna dos 10 Tópicos Mais Frequentes...\n")

p_coerencia <- ggplot(df_distancias_internas_top10,
                      aes(x = factor(topico), y = distancia, fill = factor(topico))) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Coerência Interna dos Tópicos: Distribuição da Distância JSD",
    x = "Tópico",
    y = "Distância de Jensen-Shannon (0 = idêntico, 1 = diferente)",
    fill = "Tópico"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_coerencia)
