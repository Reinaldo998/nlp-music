library(Rtsne)
library(dplyr)
library(ggplot2)
library(ggrepel)

set.seed(123)

# 1. Adiciona coluna Index para rastrear a posição original
data_final_pt <- data_final_pt %>% mutate(Index = row_number())

# 2. KMeans com 7 clusters direto na matriz TF-IDF
kmeans_tfidf <- kmeans(matriz_tfidf_filtrada, centers = 7, nstart = 25)
df_clusters_tfidf <- data.frame(ID = data_final_pt$ID, Cluster = kmeans_tfidf$cluster)

table(as.factor(kmeans_tfidf$cluster))

# 3. Prepara matriz sem duplicadas para o t-SNE e salva índices originais
matriz_unica <- unique(matriz_tfidf_filtrada)
idx_unicos <- which(!duplicated(matriz_tfidf_filtrada))

tsne_result <- Rtsne(matriz_unica, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000)
tsne_tfidf <- as.data.frame(tsne_result$Y)
colnames(tsne_tfidf) <- c("Dim1", "Dim2")
tsne_tfidf$Index <- idx_unicos

# 4. Junta tudo em um único dataframe para visualização
df_plot <- tsne_tfidf %>%
  left_join(data_final_pt, by = "Index") %>%
  left_join(df_clusters_tfidf, by = "ID")

# 5. Seleciona amostra de músicas para rotular no gráfico
set.seed(42)
rotuladas <- df_plot %>% sample_n(100)

# 6. Plot com t-SNE e cores por cluster
ggplot(df_plot, aes(x = Dim1, y = Dim2, color = factor(Cluster))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_text_repel(data = rotuladas, aes(label = Nome), size = 2.5, max.overlaps = 30) +
  theme_minimal() +
  labs(title = "t-SNE das Letras de Músicas — TF-IDF + Clusters",
       x = "Dimensão 1", y = "Dimensão 2",
       color = "Cluster")
