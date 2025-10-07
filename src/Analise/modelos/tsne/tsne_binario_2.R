#===================================
artistas <- data_final_pt$Artista.x
anos <- data_final_pt$Ano
# ===================================
# FUNÇÃO PARA APLICAR t-SNE - VERSÃO CORRIGIDA
# ===================================
aplicar_tsne_2 <- function(matriz, label_representacao, nomes_musicas = NULL, artistas = NULL, num_palavras = NULL, anos = NULL) {
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
# PREPARAÇÃO DA MATRIZ BINÁRIA
# ===================================
# Converte DTM para matriz binária (0 = ausente, 1 = presente)
matriz_binaria <- as.matrix(dtm_tf)
matriz_binaria[matriz_binaria > 0] <- 1
# Remove variáveis com variância zero (colunas que só têm 0s ou 1s)
matriz_binaria_filtrada <- remove_zero_var(matriz_binaria)
# Calcula o número de palavras únicas por música (soma das linhas da matriz binária)
num_palavras_por_musica <- rowSums(matriz_binaria_filtrada)
# ===================================
# APLICAÇÃO DO t-SNE - CHAMADA CORRETA
# ===================================
# Aplica t-SNE na representação binária com argumentos nomeados explicitamente
tsne_bin <- aplicar_tsne_2(matriz = matriz_binaria_filtrada, 
                           label_representacao = "Binária", 
                           nomes_musicas = nomes_musicas,
                           artistas = artistas,
                           num_palavras = num_palavras_por_musica,
                           anos = anos)
# ===================================
# VISUALIZAÇÃO COM PLOTLY - DOIS GRÁFICOS
# ===================================
# Define o hover text com todas as informações (sem as dimensões)
if("Musica" %in% colnames(tsne_bin)) {
  hover_text <- ~paste(
    "Música:", Musica,
    "<br>Artista:", ifelse(is.null(Artista) | is.na(Artista), "N/A", Artista),
    "<br>Palavras únicas:", NumPalavras,
    "<br>Ano:", ifelse(is.null(Ano) | is.na(Ano), "N/A", Ano)
  )
} else {
  hover_text <- ~paste(
    "Ponto:", seq_len(nrow(tsne_bin))
  )
}

# ===================================
# GRÁFICO 1: CORES POR NÚMERO DE PALAVRAS
# ===================================
grafico_palavras <- plot_ly(
  tsne_bin,
  x = ~Dim1, 
  y = ~Dim2,
  text = hover_text,
  type = 'scatter',
  mode = 'markers',
  marker = list(
    size = 8,
    color = ~NumPalavras,  # Cor baseada no número de palavras
    colorscale = list(c(0, "#FFEDA0"), c(0.5, "#FD8D3C"), c(1, "#BD0026")),  # Amarelo claro -> Laranja -> Vermelho escuro
    opacity = 0.8,
    line = list(width = 1, color = "white"),
    colorbar = list(
      title = "Número de<br>Palavras",
      titlefont = list(size = 12)
    )
  ),
  hoverinfo = 'text'
) %>% 
  layout(
    title = list(
      text = "t-SNE — Colorido por Número de Palavras",
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
# GRÁFICO 2: CORES POR ANO
# ===================================
grafico_anos <- plot_ly(
  tsne_bin,
  x = ~Dim1, 
  y = ~Dim2,
  text = hover_text,
  type = 'scatter',
  mode = 'markers',
  marker = list(
    size = 8,
    color = ~Ano,  # Cor baseada no ano
    colorscale = list(c(0, "#F7FBFF"), c(0.5, "#6BAED6"), c(1, "#08306B")),  # Azul claro -> Azul médio -> Azul escuro
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
      text = "t-SNE — Colorido por Ano",
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
# SALVAMENTO DOS GRÁFICOS INTERATIVOS
# ===================================
# Carrega a biblioteca htmlwidgets (instale se necessário: install.packages("htmlwidgets"))
library(htmlwidgets)

# Salva o gráfico de palavras como arquivo HTML interativo
saveWidget(
  grafico_palavras, 
  file = "tsne_grafico_palavras.html",
  selfcontained = TRUE  # Inclui todas as dependências no arquivo
)

# Salva o gráfico de anos como arquivo HTML interativo
saveWidget(
  grafico_anos, 
  file = "tsne_grafico_anos.html", 
  selfcontained = TRUE  # Inclui todas as dependências no arquivo
)

# Mensagem de confirmação
cat("Gráficos salvos com sucesso!\n")
cat("- tsne_grafico_palavras.html (colorido por número de palavras)\n")
cat("- tsne_grafico_anos.html (colorido por ano)\n")
cat("Os arquivos são totalmente interativos e podem ser abertos em qualquer navegador.\n")

# Exibe os gráficos (opcional)
print(grafico_palavras)
print(grafico_anos)