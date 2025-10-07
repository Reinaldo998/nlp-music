# Certifique-se de que as bibliotecas necessárias estão carregadas
library(Rtsne)
library(plotly)
library(htmlwidgets)
library(dplyr)       # Para manipulação de dados
library(factoextra)  # Para métodos de determinação de k e visualização

# ===================================
# PRÉ-REQUISITOS (certifique-se de que estas variáveis existem)
# ===================================

# ESCOLHA AQUI QUAL RESULTADO DO T-SNE VOCÊ QUER USAR:
# 1. Para t-SNE baseado em TF (Frequência de Termos):
 tsne_data_for_kmeans <- tsne_muito_frequente$Y

# 2. Para t-SNE baseado em TF-IDF (Term Frequency-Inverse Document Frequency):
tsne_data_for_kmeans <- tsne_tfidf$Y # <-- USANDO TF-IDF COMO PADRÃO NESTE EXEMPLO

# As informações auxiliares das músicas (assumindo que já estão no ambiente)
# nomes_musicas <- data_final_pt$Musica
# artistas <- data_final_pt$Artista.x
# anos <- data_final_pt$Ano
# num_palavras_tf <- rowSums(matriz_tf_t) # do seu código anterior

# ===================================
# 1. DETERMINAR O NÚMERO ÓTIMO DE CLUSTERS (k) - MÉTODOS AUXILIARES
# ===================================

cat("=== DETERMINANDO O NÚMERO ÓTIMO DE CLUSTERS (k) ===\n")
cat("Isso pode levar alguns segundos a minutos, dependendo do número de pontos.\n")

# MÉTODO DO COTOVELO (Elbow Method)
# Baseado na Soma dos Quadrados Dentro dos Clusters (WCSS)
# Olhe para o "cotovelo" da curva.
cat("\nMétodo do Cotovelo (Elbow Method):\n")
fviz_nbclust(tsne_data_for_kmeans, kmeans, method = "wss", k.max = 10) +
  labs(subtitle = "Método do Cotovelo")
# Clique na janela de plot para ver o gráfico.

# MÉTODO DA SILHUETA
# Avalia a qualidade dos clusters. Valores mais altos são melhores.
cat("\nMétodo da Silhueta:\n")
fviz_nbclust(tsne_data_for_kmeans, kmeans, method = "silhouette", k.max = 10) +
  labs(subtitle = "Método da Silhueta")
# Clique na janela de plot para ver o gráfico.

# Você também pode usar o Método da Estatística Gap:
# fviz_nbclust(tsne_data_for_kmeans, kmeans, method = "gap_stat", k.max = 10) +
#   labs(subtitle = "Estatística Gap")

cat("\nAnalise os gráficos acima para escolher o valor de 'k' mais adequado.\n")
cat("O método do Cotovelo busca um 'cotovelo' na curva.\n")
cat("O método da Silhueta busca o maior valor médio da silhueta.\n\n")

# ===================================
# 2. REALIZAR K-MEANS
# ===================================

# Defina o número de clusters (k) aqui, com base na sua análise dos gráficos acima.
# Exemplo: k = 4 (você deve ajustar este valor!)
numero_de_grupos_k <- 4# <--- ALTERE ESTE VALOR PARA O NÚMERO DE CLUSTERS DESEJADO!

cat(paste0("=== REALIZANDO K-MEANS COM k = ", numero_de_grupos_k, " ===\n"))

# Configurar semente para reprodutibilidade
set.seed(123)

# Aplicar K-Means
# X: os dados (suas coordenadas t-SNE)
# centers: o número de clusters (k)
# nstart: número de vezes que o algoritmo será executado com diferentes centróides iniciais.
#         O melhor resultado será escolhido. Um valor de 20 a 50 é comum.
kmeans_resultado <- kmeans(tsne_data_for_kmeans, centers = numero_de_grupos_k, nstart = 25)

cat("K-Means concluído!\n")
cat("Distribuição dos clusters:\n")
print(table(kmeans_resultado$cluster))

# ===================================
# 3. INTEGRAR CLUSTERS NO DATAFRAME T-SNE E PLOTAR
# ===================================

cat("\n=== GERANDO GRÁFICO T-SNE COM CLUSTERS K-MEANS ===\n")

# Criar dataframe com coordenadas t-SNE e metadados, incluindo os clusters K-Means
df_tsne_kmeans_com_clusters <- data.frame(
  TSNE1 = tsne_data_for_kmeans[, 1],
  TSNE2 = tsne_data_for_kmeans[, 2],
  Musica = nomes_musicas,
  Artista = artistas,
  Ano = anos,
  NumPalavras = num_palavras_tf,
  Cluster = factor(kmeans_resultado$cluster) # Atribuições de cluster do K-Means
)

# Criar texto para hover (incluindo o cluster)
hover_text_kmeans_cluster <- paste(
  "Música:", df_tsne_kmeans_com_clusters$Musica,
  "<br>Artista:", ifelse(is.na(df_tsne_kmeans_com_clusters$Artista), "N/A", df_tsne_kmeans_com_clusters$Artista),
  "<br>Ano:", ifelse(is.na(df_tsne_kmeans_com_clusters$Ano), "N/A", df_tsne_kmeans_com_clusters$Ano),
  "<br>Total de palavras:", ifelse(is.na(df_tsne_kmeans_com_clusters$NumPalavras), "N/A", df_tsne_kmeans_com_clusters$NumPalavras),
  "<br>Cluster:", df_tsne_kmeans_com_clusters$Cluster
)

# Gráfico t-SNE colorido pelos clusters K-Means
grafico_tsne_kmeans <- plot_ly(
  data = df_tsne_kmeans_com_clusters,
  x = ~TSNE1,
  y = ~TSNE2,
  text = hover_text_kmeans_cluster,
  type = 'scatter',
  mode = 'markers',
  color = ~Cluster, # Colore por cluster K-Means
  colors = "Set1", # Uma paleta de cores para categorias (ex: Set1, Dark2, Paired)
  marker = list(
    size = 10,
    opacity = 0.8,
    line = list(width = 1, color = "white")
  ),
  hovertemplate = paste(
    "%{text}",
    "<extra></extra>"
  )
) %>%
  layout(
    title = list(
      text = paste0("t-SNE (K-Means): Músicas Agrupadas em ", numero_de_grupos_k, " Clusters"),
      font = list(size = 18, color = "#2c3e50")
    ),
    xaxis = list(
      title = "t-SNE Componente 1",
      titlefont = list(size = 14),
      showgrid = TRUE, gridcolor = "#ecf0f1", zeroline = FALSE
    ),
    yaxis = list(
      title = "t-SNE Componente 2",
      titlefont = list(size = 14),
      showgrid = TRUE, gridcolor = "#ecf0f1", zeroline = FALSE
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest",
    font = list(family = "Arial, sans-serif")
  )

# Exibir gráfico
print(grafico_tsne_kmeans)

# ===================================
# 4. SALVAR GRÁFICO (OPCIONAL)
# ===================================

arquivo_tsne_kmeans <- paste0("tsne_kmeans_clusters_k_muito_frequente", numero_de_grupos_k, ".html")
saveWidget(
  widget = grafico_tsne_kmeans,
  file = arquivo_tsne_kmeans,
  selfcontained = TRUE,
  title = paste0("t-SNE K-Means Músicas (k=", numero_de_grupos_k, ")")
)
cat("\nGráfico t-SNE com clusters K-Means salvo em:", arquivo_tsne_kmeans, "\n")

# ===================================
# 5. ANÁLISE DOS CLUSTERS K-MEANS
# ===================================

cat("\n=== ANÁLISE DOS CLUSTERS K-MEANS ===\n")
# Adicionar a coluna de cluster ao dataframe original ou a um clone para análise
data_com_kmeans_clusters_tf <- data.frame(
  Musica = nomes_musicas,
  Artista = artistas,
  Ano = anos,
  Cluster = factor(kmeans_resultado$cluster)
)

# Exemplo: Contagem de músicas por cluster
cat("\nContagem de músicas por cluster K-Means:\n")
print(table(data_com_kmeans_clusters_tf$Cluster))

# Exemplo: Média do ano por cluster (para ver se há tendência temporal)
cat("\nMédia do ano por cluster K-Means:\n")
print(aggregate(Ano ~ Cluster, data = data_com_kmeans_clusters_tf, FUN = mean))

# Exemplo: Top 5 artistas por cluster (os mais frequentes)
cat("\nTop 5 Artistas por Cluster K-Means:\n")
data_com_kmeans_clusters_tf %>%
  group_by(Cluster, Artista) %>%
  summarise(N = n(), .groups = 'drop') %>%
  arrange(Cluster, desc(N)) %>%
  group_by(Cluster) %>%
  slice_head(n = 5) %>%
  print()

cat("\nAnálise K-Means baseada em t-SNE concluída!\n")

#============================================================

library(dplyr)
library(tidyr) # Necessário para a função pivot_wider

# Remover artistas NA, se houver
data_com_kmeans_clusters_filtrado_tf_10 <- data_com_kmeans_clusters_tf %>%
  filter(!is.na(Artista))

cat("\n=== TOP 10 ARTISTAS POR CLUSTER K-MEANS ===\n")

top_artistas_por_cluster_tf_10 <- data_com_kmeans_clusters_filtrado_tf_10 %>%
  group_by(Cluster, Artista) %>%
  summarise(Contagem = n(), .groups = 'drop_last') %>% # Conta a frequência de cada artista por cluster
  arrange(Cluster, desc(Contagem)) %>% # Ordena por cluster e, dentro de cada cluster, por contagem decrescente
  group_by(Cluster) %>%
  mutate(Ranking = row_number()) %>% # Atribui um ranking a cada artista dentro do seu cluster
  filter(Ranking <= 10) %>% # Filtra para os 10 primeiros rankings
  ungroup()

# ===================================
# 2. PIVOTAR PARA FORMATO ALINHADO
# ===================================

# Transforma os dados para que cada cluster seja uma coluna
# e os valores sejam os artistas, alinhados pelo ranking.
top_artistas_alinhado_tf_10 <- top_artistas_por_cluster_tf_10 %>%
  select(Cluster, Artista, Ranking) %>% # Seleciona apenas as colunas relevantes
  pivot_wider(
    names_from = Cluster, # Os valores únicos de 'Cluster' se tornarão nomes de colunas
    values_from = Artista, # Os valores de 'Artista' preencherão as novas colunas
    names_prefix = "Cluster_", # Adiciona um prefixo para os nomes das novas colunas (ex: Cluster_1, Cluster_2)
    id_cols = Ranking # Mantém o 'Ranking' como a coluna de identificação para as linhas
  ) %>%
  arrange(Ranking) # Ordena pelo Ranking para que o top 1 esteja na primeira linha, etc.

# ===================================
# 3. EXIBIR TABELA ALINHADA
# ===================================

cat("\nTop 10 Artistas por Cluster K-Means (Alinhado por Ranking):\n")
print(top_artistas_alinhado)

cat("\nAnálise de Top Artistas por Cluster concluída.\n")

# ===================================================================
# 6. NOVA SEÇÃO: TOP 10 PALAVRAS POR CLUSTER
# ===================================================================

cat("\n=== ANÁLISE: TOP 10 PALAVRAS POR CLUSTER K-MEANS ===\n")

# 1. Associar os clusters às músicas na matriz de termos
# Certifique-se de que 'matriz_termo_para_analise' (matriz_tfidf_t ou matriz_tf_t)
# e 'kmeans_resultado$cluster' têm o mesmo número de linhas e estão na mesma ordem.
matriz_com_cluster <- as.data.frame(matriz_tf_muito_frequente) # Converter para dataframe
matriz_com_cluster$Cluster <- factor(kmeans_resultado$cluster)

# 2. Calcular a soma dos pesos das palavras por cluster
# Isso é feito agrupando por Cluster e somando os valores de cada coluna (palavra)
# em cada cluster.
soma_pesos_palavras_por_cluster <- matriz_com_cluster %>%
  group_by(Cluster) %>%
  summarise(across(everything(), sum)) # Soma todas as colunas numéricas (palavras)

# Remover a coluna 'Cluster' temporariamente se ela não foi removida pelo summarise_across
# (caso ela tenha sido tratada como numérica por engano, o que não deve acontecer com factor)
soma_pesos_palavras_por_cluster_tidy <- soma_pesos_palavras_por_cluster %>%
  pivot_longer(
    cols = -Cluster, # Todas as colunas exceto 'Cluster'
    names_to = "Palavra",
    values_to = "Soma_Peso"
  )

# 3. Identificar as Top 10 palavras por cluster
top_palavras_por_cluster <- soma_pesos_palavras_por_cluster_tidy %>%
  group_by(Cluster) %>%
  arrange(Cluster, desc(Soma_Peso)) %>% # Ordena por cluster e, dentro de cada cluster, por peso decrescente
  mutate(Ranking = row_number()) %>% # Atribui um ranking a cada palavra dentro do seu cluster
  filter(Ranking <= 10) %>% # Filtra para os 10 primeiros rankings
  ungroup()

# 4. Criar o data.frame final (pivotar para formato alinhado)
cat("\nTop 10 Palavras por Cluster K-Means (Alinhado por Ranking):\n")
top_palavras_alinhado_tf_10 <- top_palavras_por_cluster %>%
  select(Cluster, Palavra, Ranking) %>% # Seleciona apenas as colunas relevantes
  pivot_wider(
    names_from = Cluster, # Os valores únicos de 'Cluster' se tornarão nomes de colunas
    values_from = Palavra, # Os valores de 'Palavra' preencherão as novas colunas
    names_prefix = "Cluster_", # Adiciona um prefixo para os nomes das novas colunas (ex: Cluster_1, Cluster_2)
    id_cols = Ranking # Mantém o 'Ranking' como a coluna de identificação para as linhas
  ) %>%
  arrange(Ranking) # Ordena pelo Ranking para que o top 1 esteja na primeira linha, etc.

print(top_palavras_alinhado)

cat("\nAnálise de Top Palavras por Cluster concluída.\n")


