aplicar_tsne_2 <- function(matriz, label_representacao, nomes_musicas = NULL) {
  matriz_unica <- unique(matriz)  # Remove linhas duplicadas
  tsne_result <- Rtsne(matriz_unica, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000)
  resultado <- as.data.frame(tsne_result$Y)
  colnames(resultado) <- c("Dim1", "Dim2")
  resultado$Representacao <- label_representacao
  if (!is.null(nomes_musicas)) {
    resultado$Musica <- nomes_musicas[match(rownames(matriz_unica), rownames(matriz))]  # Alinha os nomes corretamente
  }
  return(resultado)
}


matriz_binaria <- as.matrix(dtm_tf)
matriz_binaria[matriz_binaria > 0] <- 1
matriz_binaria_filtrada <- remove_zero_var(matriz_binaria)

tsne_bin <- aplicar_tsne_2(matriz_binaria_filtrada, "Binária")

ggplot(tsne_bin, aes(x = Dim1, y = Dim2)) +
  geom_point(color = "#009E73", alpha = 0.7, size = 2) +
  ggtitle("t-SNE — Representação Binária") +
  theme_minimal()
