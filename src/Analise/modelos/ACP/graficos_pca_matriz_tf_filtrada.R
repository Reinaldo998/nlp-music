# Carregar pacote plotly (se ainda não estiver carregado)
library(plotly)

# ===================================
# FUNÇÃO PARA CRIAR GRÁFICOS PLOTLY (VERSÃO SEM COS2)
# ===================================
# Esta é a versão original da função, sem a dependência de valores cos2.
# Ela usará 'Ano' ou 'NumPalavras' para colorir.

criar_grafico_pca_plotly <- function(pca_resultado, titulo, nomes_musicas, artistas, anos, num_palavras, color_by = "Ano") {
  # Extrair coordenadas das duas primeiras componentes
  coordenadas <- data.frame(
    PC1 = pca_resultado$x[,1],
    PC2 = pca_resultado$x[,2],
    NumPalavras = num_palavras,
    Musica = nomes_musicas,
    Artista = artistas,
    Ano = anos
  )
  
  # Lógica para definir a variável de cor e a paleta de cores
  color_var <- switch(color_by,
                      "Ano" = coordenadas$Ano,
                      "NumPalavras" = coordenadas$NumPalavras,
                      stop("color_by deve ser 'Ano' ou 'NumPalavras'")
  )
  
  color_title <- switch(color_by,
                        "Ano" = "Ano",
                        "NumPalavras" = "Total de<br>Palavras"
  )
  
  # Usando as mesmas colorscales do seu script t-SNE
  colorscale_chosen <- switch(color_by,
                              "Ano" = list(c(0, "#F7FBFF"), c(0.5, "#6BAED6"), c(1, "#08306B")), # Azul (para Ano)
                              "NumPalavras" = list(c(0, "#FFEDA0"), c(0.5, "#FD8D3C"), c(1, "#BD0026")) # Vermelho/Laranja (para Palavras)
  )
  
  # Calcular variância explicada
  var_explicada <- summary(pca_resultado)$importance[2, 1:2] * 100
  
  # Criar hover text
  hover_text <- ~paste(
    "Música:", Musica,
    "<br>Artista:", ifelse(is.na(Artista), "N/A", Artista),
    "<br>Total de palavras:", ifelse(is.na(NumPalavras), "N/A", NumPalavras),
    "<br>Ano:", ifelse(is.na(Ano), "N/A", Ano)
  )
  
  # Criar gráfico plotly
  grafico <- plot_ly(
    coordenadas,
    x = ~PC1,
    y = ~PC2,
    text = hover_text,
    type = 'scatter',
    mode = 'markers',
    marker = list(
      size = 8,
      color = color_var,
      colorscale = colorscale_chosen,
      opacity = 0.8,
      line = list(width = 1, color = "white"),
      colorbar = list(
        title = color_title,
        titlefont = list(size = 12)
      )
    ),
    hoverinfo = 'text'
  ) %>%
    layout(
      title = list(
        text = paste0(titulo, "<br><sub>PC1: ", round(var_explicada[1], 1),
                      "% | PC2: ", round(var_explicada[2], 1), "%</sub>"),
        font = list(size = 16, color = "black")
      ),
      xaxis = list(
        title = paste0("PC1 (", round(var_explicada[1], 1), "%)"),
        titlefont = list(size = 14),
        showgrid = TRUE,
        gridcolor = "#f0f0f0"
      ),
      yaxis = list(
        title = paste0("PC2 (", round(var_explicada[2], 1), "%)"),
        titlefont = list(size = 14),
        showgrid = TRUE,
        gridcolor = "#f0f0f0"
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      hovermode = "closest"
    )
  
  return(grafico)
}

# ===================================
# GERAÇÃO DO GRÁFICO PLOTLY PARA PCA TF FILTRADO (SEM COS2)
# ===================================

# Agora chamamos a função 'criar_grafico_pca_plotly' com o pca_tf_filtrado
# Vamos colorir por Ano por padrão.

grafico_pca_tf_filtrado_final <- criar_grafico_pca_plotly(
  pca_resultado = pca_tf_filtrado,
  titulo = "PCA: Representação TF Filtrada por Frequência — Colorido por Ano",
  num_palavras = num_palavras_tf, # Assumindo que num_palavras_tf ainda corresponde às músicas originais
  nomes_musicas = nomes_musicas,   # Assumindo alinhamento
  artistas = artistas,             # Assumindo alinhamento
  anos = anos,                     # Assumindo alinhamento
  color_by = "Ano"                 # Colorindo por Ano
)

# Adicionando os limites de zoom desejados (xlim = c(-5, 5), ylim = c(-5, 5))
grafico_pca_tf_filtrado_final_zoom <- grafico_pca_tf_filtrado_final %>%
  layout(
    xaxis = list(range = c(-5, 5)),
    yaxis = list(range = c(-5, 5))
  )

# Exibir o gráfico
print(grafico_pca_tf_filtrado_final_zoom)

# Defina o nome do arquivo para salvar o gráfico
nome_arquivo <- "pca_tf_filtrado_frequencia_zoom_plotly.html"

# Salvar o gráfico como um arquivo HTML autônomo
saveWidget(grafico_pca_tf_filtrado_final_zoom,
           file = nome_arquivo,
           selfcontained = TRUE)

cat(paste0("\nGráfico PCA interativo salvo com sucesso em: ", nome_arquivo, "\n"))
cat("Você pode abrir este arquivo em qualquer navegador web para interagir com o gráfico.\n")

cat("\nGráfico PCA interativo para a representação TF filtrada, com pontos e informações no balãozinho, gerado com sucesso.\n")
