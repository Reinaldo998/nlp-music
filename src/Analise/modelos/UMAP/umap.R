# Instalar se ainda não tiver
install.packages("uwot")
# install.packages("plotly")
library(uwot)
library(plotly)

aplicar_umap <- function(matriz, label_representacao, nomes_musicas) {
  resultado_umap <- umap(matriz, n_components = 2, n_neighbors = 15, min_dist = 0.1, metric = "euclidean")
  resultado <- as.data.frame(resultado_umap)
  colnames(resultado) <- c("Dim1", "Dim2")
  resultado$Representacao <- label_representacao
  resultado$Musica <- nomes_musicas
  return(resultado)
}

umap_tf <- aplicar_umap(matriz_tf_filtrada, "TF", rownames(matriz_tf_filtrada))
umap_tfidf <- aplicar_umap(matriz_tfidf_filtrada, "TF-IDF", rownames(matriz_tfidf_filtrada))
umap_binaria <- aplicar_umap(matriz_binaria_filtrada, "Binária", rownames(matriz_binaria_filtrada))

# Salvar
saveRDS(umap_tf, "umap_tf.rds")
saveRDS(umap_tfidf, "umap_tfidf.rds")
saveRDS(umap_binaria, "umap_binaria.rds")
#####################################################################################

umap_tf <- readRDS("umap_tf.rds")
umap_tfidf <- readRDS("umap_tfidf.rds")
umap_binaria <- readRDS("umap_binaria.rds")



plot_umap <- function(dados_umap, titulo, cor) {
  plot_ly(
    data = dados_umap,
    x = ~Dim1,
    y = ~Dim2,
    text = ~Musica,
    type = "scatter",
    mode = "markers",
    marker = list(color = cor, size = 7, opacity = 0.7)
  ) %>%
    layout(title = titulo,
           xaxis = list(title = "UMAP 1"),
           yaxis = list(title = "UMAP 2"))
}

plot_umap(umap_tf, "UMAP — Representação TF", "#00AFBB")
plot_umap(umap_tfidf, "UMAP — Representação TF-IDF", "#FC4E07")
plot_umap(umap_binaria, "UMAP — Representação Binária", "#E7B800")

###############################################################################
#### K-Means ####

set.seed(123)  # Para reprodutibilidade

# K-means em cada representação
kmeans_umap_tf <- kmeans(umap_tf[, c("Dim1", "Dim2")], centers = 7, nstart = 25)
kmeans_umap_tfidf <- kmeans(umap_tfidf[, c("Dim1", "Dim2")], centers = 7, nstart = 25)
kmeans_umap_binaria <- kmeans(umap_binaria[, c("Dim1", "Dim2")], centers = 7, nstart = 25)

# Adicionando o grupo ao dataframe
umap_tf$Grupo <- as.factor(kmeans_umap_tf$cluster)
umap_tfidf$Grupo <- as.factor(kmeans_umap_tfidf$cluster)
umap_binaria$Grupo <- as.factor(kmeans_umap_binaria$cluster)

plot_umap_kmeans <- function(dados_umap, titulo) {
  plot_ly(
    data = dados_umap,
    x = ~Dim1,
    y = ~Dim2,
    text = ~paste("Música:", Musica, "<br>Grupo:", Grupo),
    color = ~Grupo,
    colors = "Set1",
    type = "scatter",
    mode = "markers",
    marker = list(size = 7, opacity = 0.8)
  ) %>%
    layout(title = titulo,
           xaxis = list(title = "UMAP 1"),
           yaxis = list(title = "UMAP 2"))
}


plot_umap_kmeans(umap_tf, "UMAP + K-means (7 grupos) — Representação TF")
plot_umap_kmeans(umap_tfidf, "UMAP + K-means (7 grupos) — Representação TF-IDF")
plot_umap_kmeans(umap_binaria, "UMAP + K-means (7 grupos) — Representação Binária")

