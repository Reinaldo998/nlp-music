library(Rtsne)
library(ggplot2)
library(plotly)

set.seed(123)  # Para resultados reprodutíveis

aplicar_tsne_3 <- function(matriz, label_representacao, nomes_musicas) {
  matriz_unica <- unique(matriz)
  tsne_result <- Rtsne(matriz_unica, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000)
  resultado <- as.data.frame(tsne_result$Y)
  colnames(resultado) <- c("Dim1", "Dim2")
  resultado$Representacao <- label_representacao
  resultado$Musica <- nomes_musicas[match(rownames(matriz_unica), rownames(matriz))]
  return(resultado)
}


tsne_tf <- aplicar_tsne_3(matriz_tf_filtrada, "TF", nomes_musicas)
tsne_tfidf <- aplicar_tsne_3(matriz_tfidf_filtrada, "TF-IDF", nomes_musicas)

matriz_binaria <- matriz_tf_filtrada
matriz_binaria[matriz_binaria > 0] <- 1
matriz_binaria_filtrada <- remove_zero_var(matriz_binaria)
tsne_bin <- aplicar_tsne_3(matriz_binaria_filtrada, "Binária", nomes_musicas)

# Função para aplicar k-means e gerar gráfico plotly
plot_tsne_kmeans <- function(tsne_df, titulo) {
  set.seed(123)
  kmeans_result <- kmeans(tsne_df[, c("Dim1", "Dim2")], centers = 2, nstart = 25)
  tsne_df$Grupo <- as.factor(kmeans_result$cluster)
  
  plot_ly(
    tsne_df,
    x = ~Dim1, y = ~Dim2,
    color = ~Grupo,
    text = ~paste("Música:", Musica, "<br>Grupo:", Grupo),
    type = 'scatter',
    mode = 'markers',
    marker = list(size = 8, opacity = 0.7)
  ) %>% layout(
    title = titulo,
    xaxis = list(title = "Dimensão 1"),
    yaxis = list(title = "Dimensão 2")
  )
}


plot_tsne_kmeans(tsne_tf, "t-SNE com K-means — Frequência (TF)")
plot_tsne_kmeans(tsne_tfidf, "t-SNE com K-means — TF-IDF")
plot_tsne_kmeans(tsne_bin, "t-SNE com K-means — Binária")

