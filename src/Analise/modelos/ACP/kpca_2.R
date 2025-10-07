# Pacotes necessários
library(dplyr)
library(tm)
library(text2vec)
library(kernlab)
library(plotly)

# --------------------------
# PRÉ-PROCESSAMENTO
# --------------------------
preprocessar_texto <- function(textos) {
  corpus <- VCorpus(VectorSource(textos)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("pt")) %>%
    tm_map(stripWhitespace)
  
  sapply(corpus, content)
}

textos_limpos <- preprocessar_texto(data_final_pt$Letra)

# --------------------------
# ITOKEN PARA AS 3 REPRESENTAÇÕES
# --------------------------
it <- itoken(textos_limpos, progressbar = FALSE)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)

# 1. TF
it_tf <- itoken(textos_limpos, progressbar = FALSE)
dtm_tf <- create_dtm(it_tf, vectorizer)

# 2. TF-IDF
tfidf_transformer <- TfIdf$new()
dtm_tfidf <- tfidf_transformer$fit_transform(dtm_tf)

# 3. Binária
matriz_binaria <- as.matrix(dtm_tf)
matriz_binaria[matriz_binaria > 0] <- 1

# --------------------------
# FUNÇÃO PARA FILTRAR VARIÂNCIA ZERO
# --------------------------
remove_zero_var <- function(mat) {
  variancias <- apply(as.matrix(mat), 2, var)
  mat[, variancias > 0]
}

matriz_tf_filtrada     <- remove_zero_var(as.matrix(dtm_tf))
matriz_tfidf_filtrada  <- remove_zero_var(as.matrix(dtm_tfidf))
matriz_binaria_filtrada <- remove_zero_var(matriz_binaria)

# --------------------------
# KPCA (kernel RBF) PARA CADA REPRESENTAÇÃO
# --------------------------
kpca_tf     <- kpca(matriz_tf_filtrada, kernel = "rbfdot", features = 2)
kpca_tfidf  <- kpca(matriz_tfidf_filtrada, kernel = "rbfdot", features = 2)
kpca_bin    <- kpca(matriz_binaria_filtrada, kernel = "rbfdot", features = 2)

# --------------------------
# CONVERTE OS COMPONENTES PARA DATA.FRAME
# --------------------------
nome_musicas <- data_final_pt$Nome.x

df_tf <- as.data.frame(rotated(kpca_tf))
df_tfidf <- as.data.frame(rotated(kpca_tfidf))
df_bin <- as.data.frame(rotated(kpca_bin))

colnames(df_tf) <- colnames(df_tfidf) <- colnames(df_bin) <- c("Dim1", "Dim2")

df_tf$Musica <- df_tfidf$Musica <- df_bin$Musica <- nome_musicas

# --------------------------
# PLOTLY INTERATIVO PARA CADA GRÁFICO
# --------------------------
plot_tf <- plot_ly(df_tf, x = ~Dim1, y = ~Dim2, text = ~Musica, type = 'scatter', mode = 'markers',
                   marker = list(color = 'steelblue'), hoverinfo = "text") %>%
  layout(title = "Kernel PCA - Representação TF",
         xaxis = list(title = "Componente 1", range = c(-2, 2)),
         yaxis = list(title = "Componente 2", range = c(-2, 2)))

plot_tfidf <- plot_ly(df_tfidf, x = ~Dim1, y = ~Dim2, text = ~Musica, type = 'scatter', mode = 'markers',
                      marker = list(color = 'tomato'), hoverinfo = "text") %>%
  layout(title = "Kernel PCA - Representação TF-IDF",
         xaxis = list(title = "Componente 1", range = c(-2, 2)),
         yaxis = list(title = "Componente 2", range = c(-2, 2)))

plot_bin <- plot_ly(df_bin, x = ~Dim1, y = ~Dim2, text = ~Musica, type = 'scatter', mode = 'markers',
                    marker = list(color = 'darkgreen'), hoverinfo = "text") %>%
  layout(title = "Kernel PCA - Representação Binária",
         xaxis = list(title = "Componente 1", range = c(-2, 2)),
         yaxis = list(title = "Componente 2", range = c(-2, 2)))

# --------------------------
# MOSTRAR GRÁFICOS
# --------------------------
plot_tf
plot_tfidf
plot_bin

