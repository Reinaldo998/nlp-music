#install.packages("Rtsne")  # Se ainda não tiver

library(Rtsne)
library(ggplot2)

set.seed(123)  # Para resultados reprodutíveis

# -----------------------------------
# Função para aplicar t-SNE e retornar dataframe com resultado
# -----------------------------------
aplicar_tsne <- function(matriz, label_representacao) {
  matriz_unica <- unique(matriz)  # Remove linhas duplicadas
  tsne_result <- Rtsne(matriz_unica, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000)
  resultado <- as.data.frame(tsne_result$Y)
  colnames(resultado) <- c("Dim1", "Dim2")
  resultado$Representacao <- label_representacao
  return(resultado)
}

# Aplicando t-SNE para TF e TF-IDF
tsne_tf <- aplicar_tsne(matriz_tf_filtrada, "TF")
tsne_tfidf <- aplicar_tsne(matriz_tfidf_filtrada, "TF-IDF")
# Unir os dois resultados
dados_tsne <- rbind(tsne_tf, tsne_tfidf)

# -----------------------------------
# Plotagem com ggplot2
# -----------------------------------
#ggplot(dados_tsne, aes(x = Dim1, y = Dim2, color = Representacao)) +
#  geom_point(alpha = 0.7) +
#  theme_minimal() +
#  labs(title = "Visualização com t-SNE das Letras de Música",
#       x = "Dimensão 1", y = "Dimensão 2") +
#  scale_color_manual(values = c("TF" = "darkgreen", "TF-IDF" = "purple"))

ggplot(tsne_tf, aes(x = Dim1, y = Dim2)) +
  geom_point(color = "#00AFBB", alpha = 0.7, size = 2) +
  ggtitle("t-SNE — Representação TF (Frequência)") +
  theme_minimal()

ggplot(tsne_tfidf, aes(x = Dim1, y = Dim2)) +
  geom_point(color = "#FC4E07", alpha = 0.7, size = 2) +
  ggtitle("t-SNE — Representação TF-IDF") +
  theme_minimal()

#############################################

aplicar_tsne <- function(matriz, label_representacao, nomes_musicas) {
  matriz_unica <- unique(matriz)
  
  # Identifica quais linhas são únicas
  idx_unicos <- !duplicated(matriz)
  
  tsne_result <- Rtsne(matriz_unica, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000)
  resultado <- as.data.frame(tsne_result$Y)
  colnames(resultado) <- c("Dim1", "Dim2")
  resultado$Representacao <- label_representacao
  resultado$Musica <- nomes_musicas[idx_unicos]
  return(resultado)
}

tsne_tf <- aplicar_tsne(matriz_tf_filtrada, "TF", data_final_pt$Nome)
tsne_tfidf <- aplicar_tsne(matriz_tfidf_filtrada, "TF-IDF", data_final_pt$Nome)


# Substitua por qualquer música que esteja no seu dataset
musica_ref <- "Evidências"

# Encontra as coordenadas dessa música
ref_coords <- tsne_tfidf[tsne_tfidf$Musica == musica_ref, c("Dim1", "Dim2")]

# Calcula a distância para todas as outras
tsne_tfidf$Distancia <- sqrt((tsne_tfidf$Dim1 - ref_coords$Dim1)^2 +
                               (tsne_tfidf$Dim2 - ref_coords$Dim2)^2)

# Mostra as 5 mais próximas (excluindo ela mesma)
tsne_tfidf %>%
  filter(Musica != musica_ref) %>%
  arrange(Distancia) %>%
  head(5)
###############################################################################

tsne_tfidf$Ano <- data_final_pt$Ano[!duplicated(matriz_tfidf_filtrada)]

ggplot(tsne_tfidf, aes(x = Dim1, y = Dim2, color = as.factor(Ano))) +
  geom_point(alpha = 0.7, size = 2) +
  ggtitle("t-SNE — Músicas por Ano (TF-IDF)") +
  theme_minimal() +
  scale_color_viridis_d(name = "Ano")

# Garante que os anos correspondam às músicas do t-SNE (sem duplicados)
anos_unicos <- data_final_pt$Ano[!duplicated(matriz_tfidf_filtrada)]

# Agrupando por década
decadas <- floor(anos_unicos / 10) * 10
tsne_tfidf$Decada <- as.factor(decadas)


ggplot(tsne_tfidf, aes(x = Dim1, y = Dim2, color = Decada)) +
  geom_point(alpha = 0.8, size = 2) +
  ggtitle("t-SNE — Letras de Músicas por Década (TF-IDF)") +
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Paired", name = "Década") +
  theme(legend.position = "right")


#############################################
### kmeans ########
set.seed(123)  # Para reprodutibilidade
kmeans_resultado <- kmeans(tsne_tfidf[, c("Dim1", "Dim2")], centers = 7, nstart = 25)

# Adicionando os grupos ao dataframe
tsne_tfidf$Grupo <- as.factor(kmeans_resultado$cluster)

ggplot(tsne_tfidf, aes(x = Dim1, y = Dim2, color = Grupo)) +
  geom_point(alpha = 0.8, size = 2) +
  ggtitle("t-SNE com K-means (7 Grupos) — TF-IDF das Letras de Músicas") +
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Dark2", name = "Grupo K-means")

nomes_musicas <- data_final_pt$Nome.x  # ou outro vetor com os nomes



