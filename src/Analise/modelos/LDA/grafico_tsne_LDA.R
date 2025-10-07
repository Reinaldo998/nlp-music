# --- 0. Carregar Pacotes Necessários (certifique-se de ter todos) ---
library(Rtsne)   # Para a função t-SNE
library(ggplot2) # Para criar o gráfico base
library(dplyr)   # Para manipulação de dados
library(plotly)  # Para a interatividade

# --- Código para gerar dados_para_plot (repetido para clareza, mas você já deve tê-lo) ---
# Se você já rodou o código do t-SNE, pule esta seção e use o 'dados_para_plot' existente.
# Certifique-se que dtm_filtrada e data_com_topicos_novo estão disponíveis.

matriz_para_tsne <- as.matrix(dtm_filtrada)
linhas_validas <- rowSums(matriz_para_tsne) > 0
matriz_para_tsne_limpa <- matriz_para_tsne[linhas_validas, ]
data_com_topicos_tsne <- data_com_topicos_novo[linhas_validas, ]

set.seed(123)
tsne_results <- Rtsne(matriz_para_tsne_limpa, dims = 2, perplexity = 30, verbose = TRUE, pca = TRUE, check_duplicates = FALSE)

tsne_coords <- as.data.frame(tsne_results$Y)
colnames(tsne_coords) <- c("TSNE_X", "TSNE_Y")

dados_para_plot <- data_com_topicos_tsne %>%
   bind_cols(tsne_coords)

# --- 1. Criar o Gráfico ggplot2 com a estética 'text' para o balão ---
# As colunas Nome.y (nome da música), Artista.y (nome do artista) e Ano (ano)
# devem estar presentes em 'dados_para_plot'.
# 'k_novo' é o número de tópicos usado no seu modelo LDA atual.

p <- ggplot(dados_para_plot, aes(x = TSNE_X, y = TSNE_Y, color = factor(topico_lda),
                                 # Definindo o texto que aparecerá no balão (tooltip)
                                 text = paste("Música: ", Nome.y,
                                              "<br>Artista: ", Artista.y,
                                              "<br>Ano: ", Ano,
                                              "<br>Grupo: Tópico ", topico_lda))) +
  geom_point(alpha = 0.6, size = 2) +
  labs(
    title = paste0("Visualização t-SNE dos Tópicos (k = ", k_novo, ")"),
    x = "Dimensão t-SNE 1",
    y = "Dimensão t-SNE 2",
    color = "Tópico"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

# --- 2. Converter o objeto ggplot para um gráfico interativo plotly ---
cat("\n\n=============================================\n")
cat(" GRÁFICO INTERATIVO T-SNE COM INFORMAÇÕES NO BALÃO\n")
cat("=============================================\n")

plotly_tsne_plot <- ggplotly(p, tooltip = "text")

# Exibir o gráfico interativo
plotly_tsne_plot
