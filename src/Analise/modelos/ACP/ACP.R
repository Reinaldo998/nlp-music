# Carregar pacotes
library(dplyr)
library(tm)
library(slam)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(plotly)

# ----------------------------
# Função de pré-processamento
# ----------------------------
limpar_corpus <- function(texto) {
  corpus <- VCorpus(VectorSource(texto)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("pt")) %>%
    tm_map(stripWhitespace)
  return(corpus)
}

# -------------------------------
# Term-Document Matrix (TF bruto)
# -------------------------------
corpus_tf <- limpar_corpus(data_final_pt$Letra)
tdm_tf <- TermDocumentMatrix(corpus_tf)
matriz_tf <- as.matrix(tdm_tf)

# PCA com matriz binária (presença/ausência)
matriz_binaria <- matriz_tf
matriz_binaria[matriz_binaria > 0] <- 1
matriz_binaria_t <- t(matriz_binaria)

pca_bin <- prcomp(matriz_binaria_t, scale. = TRUE)

# Visualizações PCA binário
fviz_eig(pca_bin, main = "PCA - Matriz Binária")
fviz_pca_ind(pca_bin, geom.ind = "point", col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             label = "none") +
  coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5))

fviz_pca_var(pca_bin, col.var = "contrib",
             gradient.cols = c("blue", "red"), repel = TRUE)

# ---------------------------------------
# Term-Document Matrix com peso TF-IDF
# ---------------------------------------
corpus_tfidf <- limpar_corpus(data_final_pt$Letra)
tdm_tfidf <- TermDocumentMatrix(corpus_tfidf,
                                control = list(weighting = weightTfIdf))
matriz_tfidf <- as.matrix(tdm_tfidf)
matriz_tfidf_t <- t(matriz_tfidf)

# Remover termos com variância zero
variancias <- apply(matriz_tfidf_t, 2, var)
matriz_tfidf_filtrada <- matriz_tfidf_t[, variancias > 0]

# PCA com TF-IDF
pca_tfidf <- prcomp(matriz_tfidf_filtrada, scale. = TRUE)

# Visualizações PCA TF-IDF
fviz_eig(pca_tfidf, main = "PCA - TF-IDF")
fviz_pca_ind(pca_tfidf, geom.ind = "point", col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             label = "none") +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2))

#fviz_pca_var(pca_tfidf, col.var = "contrib",
            # gradient.cols = c("blue", "red"), repel = TRUE)


##################################################################################
library(plotly)

df_pca <- as.data.frame(pca_tfidf$x)
df_pca$musica <- data_final_pt$Nome.x

plot_ly(df_pca,
        x = ~PC1, y = ~PC2,
        type = 'scatter',
        mode = 'markers',
        text = ~musica,
        marker = list(size = 5),
        hoverinfo = 'text') %>%
  layout(title = "Músicas no Espaço PCA")



# Selecionar as músicas mais extremas nos dois primeiros PCs
extremos <- df_pca %>%
  slice_max(order_by = PC1, n = 3) %>%
  bind_rows(slice_min(df_pca, PC1, n = 3)) %>%
  bind_rows(slice_max(df_pca, PC2, n = 3)) %>%
  bind_rows(slice_min(df_pca, PC2, n = 3))

fviz_pca_ind(pca_tfidf, geom = "point") +
  geom_text_repel(data = extremos,
                  aes(x = PC1, y = PC2, label = musica),
                  inherit.aes = FALSE)



plot_ly(df_pca,
        x = ~PC1, y = ~PC2,
        type = 'scatter',
        mode = 'markers',
        text = ~musica,
        marker = list(size = 5),
        hoverinfo = 'text') %>%
  layout(title = "Músicas no Espaço PCA (TF-IDF)",
         xaxis = list(title = 'PC1', range = c(-2, 2)),
         yaxis = list(title = 'PC2', range = c(-2, 2)))

