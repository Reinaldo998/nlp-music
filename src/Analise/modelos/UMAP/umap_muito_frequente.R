# Carregar bibliotecas necessárias
library(uwot)      # Para UMAP
library(plotly)    # Para gráficos interativos
library(htmlwidgets) # Para salvar gráficos plotly
library(dplyr)     # Para manipulação de dados

# ===================================
# PRÉ-REQUISITOS (certifique-se de que as matrizes de termos existem)
# ===================================
# As matrizes de termos do seu código anterior:
# matriz_tf_t (Term Frequency - TF)
# matriz_tfidf_t (Term Frequency-Inverse Document Frequency - TF-IDF)

# As informações auxiliares das músicas (assumindo que já estão no ambiente)
# nomes_musicas <- data_final_pt$Musica
# artistas <- data_final_pt$Artista.x
# anos <- data_final_pt$Ano
# num_palavras_tf <- rowSums(matriz_tf_t) # do seu código anterior

# ===================================
# 1. UMAP para dados TF (Term Frequency)
# ===================================

cat("=== EXECUTANDO UMAP PARA DADOS TF ===\n")

# n_neighbors: número de vizinhos a serem considerados. Valores menores enfatizam a estrutura local, maiores a estrutura global.
# min_dist: controla o quão próximos os pontos podem estar no espaço de baixa dimensão. Valores menores resultam em agrupamentos mais compactos.
# metric: método de distância (e.g., "euclidean", "cosine"). "cosine" é muitas vezes melhor para dados de texto.
# ret_model: se TRUE, retorna o modelo para transformações futuras (não necessário para visualização simples).
# verbose: exibe o progresso.
umap_tf <- umap(
  matriz_tf_muito_frequente,
  n_components = 2,    # Reduzir para 2 dimensões para visualização
  n_neighbors = 15,    # Valor comum, ajuste conforme a densidade dos seus dados
  min_dist = 0.1,      # Valor comum, ajuste para densidade dos clusters
  metric = "cosine",   # Recomendado para dados de texto esparsos como TF
  ret_model = FALSE,
  verbose = TRUE
)

cat("UMAP para TF concluído.\n")

# Criar dataframe para o gráfico UMAP TF
df_umap_tf <- data.frame(
  UMAP1 = umap_tf[, 1],
  UMAP2 = umap_tf[, 2],
  Musica = nomes_musicas,
  Artista = artistas,
  Ano = anos,
  NumPalavras = num_palavras_tf
)

# Texto para hover no gráfico TF
hover_text_umap_tf <- paste(
  "Música:", df_umap_tf$Musica,
  "<br>Artista:", ifelse(is.na(df_umap_tf$Artista), "N/A", df_umap_tf$Artista),
  "<br>Ano:", ifelse(is.na(df_umap_tf$Ano), "N/A", df_umap_tf$Ano),
  "<br>Total de palavras:", ifelse(is.na(df_umap_tf$NumPalavras), "N/A", df_umap_tf$NumPalavras)
)

# Gerar gráfico UMAP TF
grafico_umap_tf <- plot_ly(
  data = df_umap_tf,
  x = ~UMAP1,
  y = ~UMAP2,
  text = hover_text_umap_tf,
  type = 'scatter',
  mode = 'markers',
  marker = list(
    size = 8,
    opacity = 0.7,
    line = list(width = 1, color = "white")
  ),
  hovertemplate = paste(
    "%{text}",
    "<extra></extra>"
  )
) %>%
  layout(
    title = list(
      text = "UMAP: Músicas (Baseado em TF)",
      font = list(size = 18, color = "#2c3e50")
    ),
    xaxis = list(
      title = "UMAP Componente 1",
      titlefont = list(size = 14),
      showgrid = TRUE, gridcolor = "#ecf0f1", zeroline = FALSE
    ),
    yaxis = list(
      title = "UMAP Componente 2",
      titlefont = list(size = 14),
      showgrid = TRUE, gridcolor = "#ecf0f1", zeroline = FALSE
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest",
    font = list(family = "Arial, sans-serif")
  )

# Exibir gráfico UMAP TF
print(grafico_umap_tf)

# Salvar gráfico UMAP TF
arquivo_umap_tf <- "umap_musicas_tf.html"
saveWidget(
  widget = grafico_umap_tf,
  file = arquivo_umap_tf,
  selfcontained = TRUE,
  title = "UMAP Músicas TF"
)
cat("\nGráfico UMAP para TF salvo em:", arquivo_umap_tf, "\n")

---
  
  # ===================================
# 2. UMAP para dados TF-IDF (Term Frequency-Inverse Document Frequency)
# ===================================

cat("\n=== EXECUTANDO UMAP PARA DADOS TF-IDF ===\n")

umap_tfidf <- umap(
  matriz_tfidf,
  n_components = 2,
  n_neighbors = 15,
  min_dist = 0.1,
  metric = "cosine", # Recomendado para dados de texto esparsos como TF-IDF
  ret_model = FALSE,
  verbose = TRUE
)

cat("UMAP para TF-IDF concluído.\n")

# Criar dataframe para o gráfico UMAP TF-IDF
df_umap_tfidf <- data.frame(
  UMAP1 = umap_tfidf[, 1],
  UMAP2 = umap_tfidf[, 2],
  Musica = nomes_musicas,
  Artista = artistas,
  Ano = anos,
  NumPalavras = num_palavras_tf
)

# Texto para hover no gráfico TF-IDF
hover_text_umap_tfidf <- paste(
  "Música:", df_umap_tfidf$Musica,
  "<br>Artista:", ifelse(is.na(df_umap_tfidf$Artista), "N/A", df_umap_tfidf$Artista),
  "<br>Ano:", ifelse(is.na(df_umap_tfidf$Ano), "N/A", df_umap_tfidf$Ano),
  "<br>Total de palavras:", ifelse(is.na(df_umap_tfidf$NumPalavras), "N/A", df_umap_tfidf$NumPalavras)
)

# Gerar gráfico UMAP TF-IDF
grafico_umap_tfidf <- plot_ly(
  data = df_umap_tfidf,
  x = ~UMAP1,
  y = ~UMAP2,
  text = hover_text_umap_tfidf,
  type = 'scatter',
  mode = 'markers',
  marker = list(
    size = 8,
    opacity = 0.7,
    line = list(width = 1, color = "white")
  ),
  hovertemplate = paste(
    "%{text}",
    "<extra></extra>"
  )
) %>%
  layout(
    title = list(
      text = "UMAP: Músicas (Baseado em TF-IDF)",
      font = list(size = 18, color = "#2c3e50")
    ),
    xaxis = list(
      title = "UMAP Componente 1",
      titlefont = list(size = 14),
      showgrid = TRUE, gridcolor = "#ecf0f1", zeroline = FALSE
    ),
    yaxis = list(
      title = "UMAP Componente 2",
      titlefont = list(size = 14),
      showgrid = TRUE, gridcolor = "#ecf0f1", zeroline = FALSE
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest",
    font = list(family = "Arial, sans-serif")
  )

# Exibir gráfico UMAP TF-IDF
print(grafico_umap_tfidf)

# Salvar gráfico UMAP TF-IDF
arquivo_umap_tfidf <- "umap_musicas_tfidf.html"
saveWidget(
  widget = grafico_umap_tfidf,
  file = arquivo_umap_tfidf,
  selfcontained = TRUE,
  title = "UMAP Músicas TF-IDF"
)
cat("\nGráfico UMAP para TF-IDF salvo em:", arquivo_umap_tfidf, "\n")

cat("\nAmbos os gráficos UMAP (TF e TF-IDF) foram gerados e salvos.\n")