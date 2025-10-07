#install.packages("kernlab")     # Para o kpca
#install.packages("text2vec")

library(dplyr)
library(tm)
library(text2vec)
library(kernlab)
library(ggplot2)

# Supondo que 'data_final_pt$Letra' contém as letras em português

# --------------------------
# FUNÇÃO DE PRÉ-PROCESSAMENTO
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

# Pré-processamento
textos_limpos <- preprocessar_texto(data_final_pt$Letra)

# --------------------------
# REPRESENTAÇÃO 1: Term Frequency (TF)
# --------------------------
it <- itoken(textos_limpos, progressbar = FALSE)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
dtm_tf <- create_dtm(it, vectorizer)

# --------------------------
# REPRESENTAÇÃO 2: TF-IDF
# --------------------------
tfidf_transformer <- TfIdf$new()
dtm_tfidf <- tfidf_transformer$fit_transform(dtm_tf)

# --------------------------
# FILTRAR COLUNAS COM VARIÂNCIA ZERO
# --------------------------
remove_zero_var <- function(mat) {
  variancias <- apply(as.matrix(mat), 2, var)
  mat[, variancias > 0]
}

matriz_tf_filtrada <- remove_zero_var(as.matrix(dtm_tf))
matriz_tfidf_filtrada <- remove_zero_var(as.matrix(dtm_tfidf))

# --------------------------
# KPCA COM KERNEL GAUSSIANO (RBF)
# --------------------------
kpca_tf <- kpca(matriz_tf_filtrada, kernel = "rbfdot", features = 2)
kpca_tfidf <- kpca(matriz_tfidf_filtrada, kernel = "rbfdot", features = 2)

# --------------------------
# EXTRAIR OS COMPONENTES
# --------------------------
comp_tf <- as.data.frame(rotated(kpca_tf))
comp_tfidf <- as.data.frame(rotated(kpca_tfidf))

# --------------------------
# PLOTAGEM DOS RESULTADOS
# --------------------------
comp_tf$Representacao <- "TF"
comp_tfidf$Representacao <- "TF-IDF"
colnames(comp_tf)[1:2] <- colnames(comp_tfidf)[1:2] <- c("Dim1", "Dim2")

dados_kpca <- rbind(comp_tf, comp_tfidf)

ggplot(dados_kpca, aes(x = Dim1, y = Dim2, color = Representacao)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Kernel PCA (RBF) nas Letras de Música",
       x = "Componente 1", y = "Componente 2") +
  scale_color_manual(values = c("TF" = "steelblue", "TF-IDF" = "tomato"))
################################################################################

#---------------------------
# scree plot
#---------------------------

# Função para calcular proporção explicada
kpca_variancia_explicada <- function(kpca_obj) {
  eigs <- kpca_obj@eig
  proporcao <- eigs / sum(eigs)
  data.frame(Componente = seq_along(proporcao),
             Proporcao = proporcao,
             Acumulada = cumsum(proporcao))
}

# Aplicar para os 3 KPCA
var_tf    <- kpca_variancia_explicada(kpca_tf)
var_tfidf <- kpca_variancia_explicada(kpca_tfidf)
var_bin   <- kpca_variancia_explicada(kpca_bin)

# Adiciona rótulo
var_tf$Tipo <- "TF"
var_tfidf$Tipo <- "TF-IDF"
var_bin$Tipo <- "Binária"

# Junta tudo
df_variancia <- bind_rows(var_tf, var_tfidf, var_bin)

# Plot
ggplot(df_variancia, aes(x = Componente, y = Proporcao, color = Tipo)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Proporção Aproximada da Variância Explicada por Componente (KPCA)",
       x = "Componente",
       y = "Proporção da Variância Explicada") +
  scale_color_manual(values = c("TF" = "steelblue", "TF-IDF" = "tomato", "Binária" = "darkgreen"))

