# Carregando bibliotecas necessárias
library(Rtsne)
library(ggplot2)
library(plotly)
library(htmlwidgets)
set.seed(123)  # Para resultados reprodutíveis

#===================================
artistas <- data_final_pt$Artista.x
anos <- data_final_pt$Ano
# ===================================
# FUNÇÃO PARA APLICAR t-SNE - VERSÃO ATUALIZADA
# ===================================
aplicar_tsne_completo <- function(matriz, label_representacao, nomes_musicas = NULL, artistas = NULL, num_palavras = NULL, anos = NULL) {
  # Remove linhas duplicadas para evitar problemas no t-SNE
  matriz_unica <- unique(matriz)
  
  # Aplica o algoritmo t-SNE
  tsne_result <- Rtsne(matriz_unica, 
                       dims = 2,           # Reduz para 2 dimensões
                       perplexity = 30,    # Controla o foco local vs global
                       verbose = TRUE,     # Mostra progresso
                       max_iter = 1000)    # Máximo de iterações
  
  # Converte resultado para dataframe
  resultado <- as.data.frame(tsne_result$Y)
  colnames(resultado) <- c("Dim1", "Dim2")
  resultado$Representacao <- label_representacao
  
  # Alinha os índices corretamente para os dados únicos
  indices_unicos <- match(rownames(matriz_unica), rownames(matriz))
  
  # Adiciona informações se fornecidas
  if (!is.null(nomes_musicas)) {
    resultado$Musica <- nomes_musicas[indices_unicos]
  }
  if (!is.null(artistas)) {
    resultado$Artista <- artistas[indices_unicos]
  }
  if (!is.null(num_palavras)) {
    resultado$NumPalavras <- num_palavras[indices_unicos]
  }
  if (!is.null(anos)) {
    resultado$Ano <- anos[indices_unicos]
  }
  
  return(resultado)
}

# ===================================
# PREPARAÇÃO DAS MATRIZES E CÁLCULOS
# ===================================
# Calcula o número total de palavras por música (soma das linhas da matriz TF)
num_palavras_tf <- rowSums(matriz_tf_filtrada)

# ===================================
# APLICAÇÃO DO t-SNE PARA TF E TF-IDF
# ===================================
# t-SNE para TF
tsne_tf <- aplicar_tsne_completo(matriz = matriz_tf_filtrada, 
                                 label_representacao = "TF", 
                                 nomes_musicas = nomes_musicas,
                                 artistas = artistas,
                                 num_palavras = num_palavras_tf,
                                 anos = anos)

# t-SNE para TF-IDF
tsne_tfidf <- aplicar_tsne_completo(matriz = matriz_tfidf_filtrada, 
                                    label_representacao = "TF-IDF", 
                                    nomes_musicas = nomes_musicas,
                                    artistas = artistas,
                                    num_palavras = num_palavras_tf,
                                    anos = anos)

# ===================================
# GRÁFICOS PARA REPRESENTAÇÃO TF
# ===================================
# Define o hover text
hover_text_tf <- ~paste(
  "Música:", Musica,
  "<br>Artista:", ifelse(is.null(Artista) | is.na(Artista), "N/A", Artista),
  "<br>Total de palavras:", NumPalavras,
  "<br>Ano:", ifelse(is.null(Ano) | is.na(Ano), "N/A", Ano)
)

# Gráfico TF colorido por número de palavras
grafico_tf_palavras <- plot_ly(
  tsne_tf,
  x = ~Dim1, 
  y = ~Dim2,
  text = hover_text_tf,
  type = 'scatter',
  mode = 'markers',
  marker = list(
    size = 8,
    color = ~NumPalavras,
    colorscale = list(c(0, "#FFEDA0"), c(0.5, "#FD8D3C"), c(1, "#BD0026")),  # Amarelo claro -> Laranja -> Vermelho escuro (MESMA DO BINÁRIO)
    opacity = 0.8,
    line = list(width = 1, color = "white"),
    colorbar = list(
      title = "Total de<br>Palavras",
      titlefont = list(size = 12)
    )
  ),
  hoverinfo = 'text'
) %>% 
  layout(
    title = list(
      text = "t-SNE TF — Colorido por Total de Palavras",
      font = list(size = 16, color = "black")
    ),
    xaxis = list(
      title = "Dimensão 1",
      titlefont = list(size = 14),
      showgrid = TRUE,
      gridcolor = "#f0f0f0"
    ),
    yaxis = list(
      title = "Dimensão 2", 
      titlefont = list(size = 14),
      showgrid = TRUE,
      gridcolor = "#f0f0f0"
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest"
  )

# Gráfico TF colorido por ano
grafico_tf_anos <- plot_ly(
  tsne_tf,
  x = ~Dim1, 
  y = ~Dim2,
  text = hover_text_tf,
  type = 'scatter',
  mode = 'markers',
  marker = list(
    size = 8,
    color = ~Ano,
    colorscale = list(c(0, "#F7FBFF"), c(0.5, "#6BAED6"), c(1, "#08306B")),  # Azul claro -> Azul médio -> Azul escuro (MESMA DO BINÁRIO)
    opacity = 0.8,
    line = list(width = 1, color = "white"),
    colorbar = list(
      title = "Ano",
      titlefont = list(size = 12)
    )
  ),
  hoverinfo = 'text'
) %>% 
  layout(
    title = list(
      text = "t-SNE TF — Colorido por Ano",
      font = list(size = 16, color = "black")
    ),
    xaxis = list(
      title = "Dimensão 1",
      titlefont = list(size = 14),
      showgrid = TRUE,
      gridcolor = "#f0f0f0"
    ),
    yaxis = list(
      title = "Dimensão 2", 
      titlefont = list(size = 14),
      showgrid = TRUE,
      gridcolor = "#f0f0f0"
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest"
  )

# ===================================
# GRÁFICOS PARA REPRESENTAÇÃO TF-IDF
# ===================================
# Define o hover text
hover_text_tfidf <- ~paste(
  "Música:", Musica,
  "<br>Artista:", ifelse(is.null(Artista) | is.na(Artista), "N/A", Artista),
  "<br>Total de palavras:", NumPalavras,
  "<br>Ano:", ifelse(is.null(Ano) | is.na(Ano), "N/A", Ano)
)

# Gráfico TF-IDF colorido por número de palavras
grafico_tfidf_palavras <- plot_ly(
  tsne_tfidf,
  x = ~Dim1, 
  y = ~Dim2,
  text = hover_text_tfidf,
  type = 'scatter',
  mode = 'markers',
  marker = list(
    size = 8,
    color = ~NumPalavras,
    colorscale = list(c(0, "#FFEDA0"), c(0.5, "#FD8D3C"), c(1, "#BD0026")),  # Amarelo claro -> Laranja -> Vermelho escuro (MESMA DO BINÁRIO)
    opacity = 0.8,
    line = list(width = 1, color = "white"),
    colorbar = list(
      title = "Total de<br>Palavras",
      titlefont = list(size = 12)
    )
  ),
  hoverinfo = 'text'
) %>% 
  layout(
    title = list(
      text = "t-SNE TF-IDF — Colorido por Total de Palavras",
      font = list(size = 16, color = "black")
    ),
    xaxis = list(
      title = "Dimensão 1",
      titlefont = list(size = 14),
      showgrid = TRUE,
      gridcolor = "#f0f0f0"
    ),
    yaxis = list(
      title = "Dimensão 2", 
      titlefont = list(size = 14),
      showgrid = TRUE,
      gridcolor = "#f0f0f0"
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest"
  )

# Gráfico TF-IDF colorido por ano
grafico_tfidf_anos <- plot_ly(
  tsne_tfidf,
  x = ~Dim1, 
  y = ~Dim2,
  text = hover_text_tfidf,
  type = 'scatter',
  mode = 'markers',
  marker = list(
    size = 8,
    color = ~Ano,
    colorscale = list(c(0, "#F7FBFF"), c(0.5, "#6BAED6"), c(1, "#08306B")),  # Azul claro -> Azul médio -> Azul escuro (MESMA DO BINÁRIO)
    opacity = 0.8,
    line = list(width = 1, color = "white"),
    colorbar = list(
      title = "Ano",
      titlefont = list(size = 12)
    )
  ),
  hoverinfo = 'text'
) %>% 
  layout(
    title = list(
      text = "t-SNE TF-IDF — Colorido por Ano",
      font = list(size = 16, color = "black")
    ),
    xaxis = list(
      title = "Dimensão 1",
      titlefont = list(size = 14),
      showgrid = TRUE,
      gridcolor = "#f0f0f0"
    ),
    yaxis = list(
      title = "Dimensão 2", 
      titlefont = list(size = 14),
      showgrid = TRUE,
      gridcolor = "#f0f0f0"
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest"
  )

# ===================================
# EXIBIÇÃO DOS GRÁFICOS
# ===================================
print(grafico_tf_palavras)
print(grafico_tf_anos)
print(grafico_tfidf_palavras)
print(grafico_tfidf_anos)

# ===================================
# SALVAMENTO DOS GRÁFICOS (COMENTADO)
# ===================================
# # Salva os gráficos TF
 saveWidget(
   grafico_tf_palavras, 
   file = "tsne_tf_palavras.html",
   selfcontained = TRUE
 )
 
 saveWidget(
   grafico_tf_anos, 
   file = "tsne_tf_anos.html", 
   selfcontained = TRUE
 )
 
 # Salva os gráficos TF-IDF
 saveWidget(
   grafico_tfidf_palavras, 
   file = "tsne_tfidf_palavras.html",
   selfcontained = TRUE
 )
 
 saveWidget(
   grafico_tfidf_anos, 
   file = "tsne_tfidf_anos.html", 
   selfcontained = TRUE
 )
 
 cat("Gráficos TF e TF-IDF salvos com sucesso!\n")
 cat("Arquivos gerados:\n")
 cat("- tsne_tf_palavras.html\n")
 cat("- tsne_tf_anos.html\n") 
 cat("- tsne_tfidf_palavras.html\n")
 cat("- tsne_tfidf_anos.html\n")