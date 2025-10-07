# Certifique-se de que as bibliotecas necessárias estão carregadas
library(dplyr)
library(ggplot2)
library(plotly) # Para tornar o ggplot interativo, se desejado

# ===================================
# PRÉ-REQUISITOS (certifique-se de que 'data_com_kmeans_clusters' existe)
# ===================================

# Se você não executou o código anterior do K-Means e 'data_com_kmeans_clusters' não está no ambiente,
# precisaria criar ele novamente ou carregar de alguma forma.
# Exemplo (se 'data_final_pt' estiver disponível e 'kmeans_resultado' do código anterior):
# data_com_kmeans_clusters <- data.frame(
#   Musica = nomes_musicas,
#   Artista = artistas,
#   Ano = anos,
#   Cluster = factor(kmeans_resultado$cluster)
# )

# Verifique se o dataframe existe e tem as colunas necessárias
if (!exists("data_com_kmeans_clusters") || !("Ano" %in% names(data_com_kmeans_clusters_tf_idf)) || !("Cluster" %in% names(data_com_kmeans_clusters_tf_idf))) {
  stop("O dataframe 'data_com_kmeans_clusters' não foi encontrado ou não contém as colunas 'Ano' e 'Cluster'. Por favor, execute o código do K-Means primeiro.")
}


# ===================================
# 1. CALCULAR A PORCENTAGEM DE CADA CLUSTER POR ANO
# ===================================

cat("=== CALCULANDO PORCENTAGEM DE CADA CLUSTER POR ANO ===\n")

# Agrupar por Ano e Cluster, contar músicas, depois calcular a porcentagem anual
df_percentagens_clusters <- data_com_kmeans_clusters_tf_idf %>%
  # Remover músicas sem ano (se houver NA em Ano) para evitar erros no agrupamento
  filter(!is.na(Ano)) %>%
  group_by(Ano, Cluster) %>%
  summarise(Contagem = n(), .groups = 'drop_last') %>% # Contar músicas por ano e cluster
  mutate(Total_Ano = sum(Contagem)) %>%               # Calcular total de músicas para cada ano
  ungroup() %>%
  mutate(Porcentagem = (Contagem / Total_Ano) * 100) # Calcular porcentagem

cat("Porcentagens calculadas:\n")
print(head(df_percentagens_clusters))

# ===================================
# 2. CRIAR GRÁFICO DE LINHAS (SÉRIE TEMPORAL)
# ===================================

cat("\n=== GERANDO GRÁFICO DE LINHAS DA PORCENTAGEM DE CLUSTERS POR ANO ===\n")

grafico_linhas_clusters <- ggplot(df_percentagens_clusters, aes(x = Ano, y = Porcentagem, color = Cluster, group = Cluster)) +
  geom_line(size = 1) + # Linhas para cada cluster
  geom_point(size = 2, aes(text = paste0("Ano: ", Ano,
                                         "<br>Cluster: ", Cluster,
                                         "<br>Porcentagem: ", round(Porcentagem, 2), "%",
                                         "<br>Músicas: ", Contagem))) + # Pontos para cada ano/cluster
  labs(
    title = "Porcentagem de Músicas por Cluster ao Longo dos Anos",
    x = "Ano",
    y = "Porcentagem (%)",
    color = "Cluster K-Means"
  ) +
  scale_x_continuous(breaks = unique(df_percentagens_clusters$Ano)) + # Garante que todos os anos sejam mostrados no eixo X
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + # Formata o eixo Y como porcentagem
  theme_minimal(base_size = 14) + # Tema minimalista com tamanho de fonte base
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1), # Gira rótulos do eixo X para melhor leitura
    legend.position = "right"
  )

# Exibir o gráfico (ggplot2)
print(grafico_linhas_clusters)

###################################################


###################################################


# Certifique-se de que as bibliotecas necessárias estão carregadas
library(dplyr)
library(ggplot2)
# library(plotly) # Não carregaremos plotly para esta versão

# ===================================
# PRÉ-REQUISITOS (certifique-se de que 'data_com_kmeans_clusters' existe)
# ===================================

# Se você não executou o código anterior do K-Means e 'data_com_kmeans_clusters' não está no ambiente,
# precisaria criar ele novamente ou carregar de alguma forma.
# Exemplo (se 'data_final_pt' estiver disponível e 'kmeans_resultado' do código anterior):
# data_com_kmeans_clusters <- data.frame(
#   Musica = nomes_musicas,
#   Artista = artistas,
#   Ano = anos,
#   Cluster = factor(kmeans_resultado$cluster)
# )

# Verifique se o dataframe existe e tem as colunas necessárias
if (!exists("data_com_kmeans_clusters") || !("Ano" %in% names(data_com_kmeans_clusters)) || !("Cluster" %in% names(data_com_kmeans_clusters))) {
  stop("O dataframe 'data_com_kmeans_clusters' não foi encontrado ou não contém as colunas 'Ano' e 'Cluster'. Por favor, execute o código do K-Means primeiro.")
}


# ===================================
# 1. CALCULAR A PORCENTAGEM DE CADA CLUSTER POR ANO
# ===================================

cat("=== CALCULANDO PORCENTAGEM DE CADA CLUSTER POR ANO ===\n")

# Agrupar por Ano e Cluster, contar músicas, depois calcular a porcentagem anual
df_percentagens_clusters <- data_com_kmeans_clusters %>%
  # Remover músicas sem ano (se houver NA em Ano) para evitar erros no agrupamento
  filter(!is.na(Ano)) %>%
  group_by(Ano, Cluster) %>%
  summarise(Contagem = n(), .groups = 'drop_last') %>% # Contar músicas por ano e cluster
  mutate(Total_Ano = sum(Contagem)) %>%               # Calcular total de músicas para cada ano
  ungroup() %>%
  mutate(Porcentagem = (Contagem / Total_Ano) * 100) # Calcular porcentagem

cat("Porcentagens calculadas (primeiras linhas):\n")
print(head(df_percentagens_clusters))

# ===================================
# 2. CRIAR GRÁFICO DE LINHAS (SÉRIE TEMPORAL) - APENAS GGPLOT2
# ===================================

cat("\n=== GERANDO GRÁFICO DE LINHAS DA PORCENTAGEM DE CLUSTERS POR ANO (GGPLOT2) ===\n")

# Determinar o ano mínimo e máximo nos dados para o eixo X
min_ano_data <- min(df_percentagens_clusters$Ano, na.rm = TRUE)
max_ano_data <- max(df_percentagens_clusters$Ano, na.rm = TRUE)

grafico_linhas_clusters_statico <- ggplot(df_percentagens_clusters, aes(x = Ano, y = Porcentagem, color = Cluster, group = Cluster)) +
  geom_line(size = 1) + # Linhas para cada cluster
  geom_point(size = 2) + # Pontos para cada ano/cluster (sem hover text para despoluir)
  labs(
    title = "Porcentagem de Músicas por Cluster ao Longo dos Anos",
    x = "Ano",
    y = "Porcentagem (%)",
    color = "Cluster K-Means"
  ) +
  # Ajustar o eixo X para começar em 1960 e mostrar de 5 em 5 anos
  scale_x_continuous(
    limits = c(min_ano_data, max_ano_data), # Definir limites do eixo X
    breaks = seq(1960, max_ano_data, by = 5) # Quebras a cada 5 anos
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + # Formata o eixo Y como porcentagem
  theme_minimal(base_size = 14) + # Tema minimalista com tamanho de fonte base
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1), # Gira rótulos do eixo X para melhor leitura
    legend.position = "right",
    panel.grid.major.x = element_line(colour = "grey90", linetype = "dashed"), # Adiciona linhas de grade mais suaves
    panel.grid.minor.x = element_blank() # Remove linhas de grade menores para despoluir
  )

# Exibir o gráfico (ggplot2 estático)
print(grafico_linhas_clusters_statico)

# ===================================
# 3. SALVAR GRÁFICO (OPCIONAL)
# ===================================

# Salvar o gráfico como uma imagem (PNG, PDF, etc.)
arquivo_grafico_percentagem_clusters_png <- "porcentagem_clusters_por_ano_statico.png"
ggsave(
  filename = arquivo_grafico_percentagem_clusters_png,
  plot = grafico_linhas_clusters_statico,
  width = 12, # Largura em polegadas
  height = 7, # Altura em polegadas
  dpi = 300   # Resolução
)
cat("\nGráfico estático salvo em:", arquivo_grafico_percentagem_clusters_png, "\n")

cat("\nAnálise de série temporal por cluster concluída com gráfico estático e eixo X ajustado!\n")

#######################

# Verifique se há NAs na coluna Porcentagem
sum(is.na(df_percentagens_clusters$Porcentagem))

# Verifique se há NAs na coluna Cluster (improvável, pois é um fator)
sum(is.na(df_percentagens_clusters$Cluster))

# Verifique se há anos antes de 1960 que foram removidos pela escala do gráfico
sum(df_percentagens_clusters$Ano < 1960)
