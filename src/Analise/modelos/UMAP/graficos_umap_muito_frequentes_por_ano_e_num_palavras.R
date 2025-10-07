# Carregar bibliotecas necessárias
library(uwot)      # Para UMAP
library(plotly)    # Para gráficos interativos
library(htmlwidgets) # Para salvar gráficos plotly
library(dplyr)     # Para manipulação de dados
library(viridis)
# library(Rtsne) # Se você precisar rodar o t-SNE novamente, descomente

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
# Executar UMAP (se ainda não o fez na sessão atual)
# ===================================
# Re-executando UMAPs para garantir que as variáveis existam
cat("Verificando/Executando UMAP para TF e TF-IDF...\n")

if (!exists("umap_tf")) {
  cat("Executando UMAP para TF...\n")
  umap_tf <- umap(
    matriz_tf_muito_frequente, n_components = 2, n_neighbors = 15, min_dist = 0.1, metric = "cosine", verbose = FALSE
  )
}

if (!exists("umap_tfidf")) {
  cat("Executando UMAP para TF-IDF...\n")
  umap_tfidf <- umap(
    matriz_tfidf, n_components = 2, n_neighbors = 15, min_dist = 0.1, metric = "cosine", verbose = FALSE
  )
}

# Criar dataframes UMAP (se ainda não os criou na sessão atual)
if (!exists("df_umap_tf")) {
  df_umap_tf <- data.frame(
    UMAP1 = umap_tf[, 1],
    UMAP2 = umap_tf[, 2],
    Musica = nomes_musicas,
    Artista = artistas,
    Ano = anos,
    NumPalavras = num_palavras_tf
  )
}

if (!exists("df_umap_tfidf")) {
  df_umap_tfidf <- data.frame(
    UMAP1 = umap_tfidf[, 1],
    UMAP2 = umap_tfidf[, 2],
    Musica = nomes_musicas,
    Artista = artistas,
    Ano = anos,
    NumPalavras = num_palavras_tf
  )
}

cat("Dataframes UMAP preparados.\n")


# ===================================
# 1. Gráficos UMAP TF com cor por ANO
# ===================================
cat("\n=== GERANDO GRÁFICO UMAP (TF) COM COR POR ANO ===\n")

# Texto para hover no gráfico TF (com Ano)
hover_text_umap_tf_ano <- paste(
  "Música:", df_umap_tf$Musica,
  "<br>Artista:", ifelse(is.na(df_umap_tf$Artista), "N/A", df_umap_tf$Artista),
  "<br>Ano:", ifelse(is.na(df_umap_tf$Ano), "N/A", df_umap_tf$Ano),
  "<br>Total de palavras:", ifelse(is.na(df_umap_tf$NumPalavras), "N/A", df_umap_tf$NumPalavras)
)

grafico_umap_tf_ano <- plot_ly(
  data = df_umap_tf,
  x = ~UMAP1,
  y = ~UMAP2,
  text = hover_text_umap_tf_ano,
  type = 'scatter',
  mode = 'markers',
  color = ~Ano, # Colore por Ano
  colors = "Blues", # Paleta de cores em gradiente (ex: "Blues", "Greens", "Reds", "YlOrRd")
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
      text = "UMAP: Músicas (TF) - Coloração por Ano",
      font = list(size = 18, color = "#2c3e50")
    ),
    xaxis = list(title = "UMAP Componente 1"),
    yaxis = list(title = "UMAP Componente 2"),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest",
    font = list(family = "Arial, sans-serif")
  )

print(grafico_umap_tf_ano)
saveWidget(widget = grafico_umap_tf_ano, file = "umap_tf_cor_por_ano.html", selfcontained = TRUE)
cat("Gráfico UMAP TF colorido por Ano salvo em: umap_tf_cor_por_ano.html\n")

# ===================================
# 2. Gráficos UMAP TF-IDF com cor por ANO
# ===================================
cat("\n=== GERANDO GRÁFICO UMAP (TF-IDF) COM COR POR ANO ===\n")

# Texto para hover no gráfico TF-IDF (com Ano)
hover_text_umap_tfidf_ano <- paste(
  "Música:", df_umap_tfidf$Musica,
  "<br>Artista:", ifelse(is.na(df_umap_tfidf$Artista), "N/A", df_umap_tfidf$Artista),
  "<br>Ano:", ifelse(is.na(df_umap_tfidf$Ano), "N/A", df_umap_tfidf$Ano),
  "<br>Total de palavras:", ifelse(is.na(df_umap_tfidf$NumPalavras), "N/A", df_umap_tfidf$NumPalavras)
)

grafico_umap_tfidf_ano <- plot_ly(
  data = df_umap_tfidf,
  x = ~UMAP1,
  y = ~UMAP2,
  text = hover_text_umap_tfidf_ano,
  type = 'scatter',
  mode = 'markers',
  color = ~Ano, # Colore por Ano
  colors = "Blues", # Outra paleta de gradiente popular (ex: "Viridis", "Plasma", "Magma", "Inferno")
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
      text = "UMAP: Músicas (TF-IDF) - Coloração por Ano",
      font = list(size = 18, color = "#2c3e50")
    ),
    xaxis = list(title = "UMAP Componente 1"),
    yaxis = list(title = "UMAP Componente 2"),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest",
    font = list(family = "Arial, sans-serif")
  )

print(grafico_umap_tfidf_ano)
saveWidget(widget = grafico_umap_tfidf_ano, file = "umap_tfidf_cor_por_ano.html", selfcontained = TRUE)
cat("Gráfico UMAP TF-IDF colorido por Ano salvo em: umap_tfidf_cor_por_ano.html\n")

#==============================================================================================


# ===================================
# 1. Gráficos UMAP TF com cor por ANO
# ===================================
cat("\n=== GERANDO GRÁFICO UMAP (TF) COM COR POR ANO ===\n")

# Texto para hover no gráfico TF (com Ano)
hover_text_umap_tf_ano <- paste(
  "Música:", df_umap_tf$Musica,
  "<br>Artista:", ifelse(is.na(df_umap_tf$Artista), "N/A", df_umap_tf$Artista),
  "<br>Ano:", ifelse(is.na(df_umap_tf$Ano), "N/A", df_umap_tf$Ano),
  "<br>Total de palavras:", ifelse(is.na(df_umap_tf$NumPalavras), "N/A", df_umap_tf$NumPalavras)
)

grafico_umap_tf_ano <- plot_ly(
  data = df_umap_tf,
  x = ~UMAP1,
  y = ~UMAP2,
  text = hover_text_umap_tf_ano,
  type = 'scatter',
  mode = 'markers',
  color = ~Ano, # Colore por Ano
  marker = list(
    size = 8,
    opacity = 0.7,
    line = list(width = 1, color = "white"),
    colorscale = scale_viridis, # Usando a paleta 'viridis'
    cmin = min(df_umap_tf$Ano, na.rm = TRUE),
    cmax = max(df_umap_tf$Ano, na.rm = TRUE),
    colorbar = list(title = "Ano")
  ),
  hovertemplate = paste(
    "%{text}",
    "<extra></extra>"
  )
) %>%
  layout(
    title = list(
      text = "UMAP: Músicas (TF) - Coloração por Ano",
      font = list(size = 18, color = "#2c3e50")
    ),
    xaxis = list(title = "UMAP Componente 1"),
    yaxis = list(title = "UMAP Componente 2"),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest",
    font = list(family = "Arial, sans-serif")
  )

print(grafico_umap_tf_ano)
saveWidget(widget = grafico_umap_tf_ano, file = "umap_tf_cor_por_ano.html", selfcontained = TRUE)
cat("Gráfico UMAP TF colorido por Ano salvo em: umap_tf_cor_por_ano.html\n")

# ===================================
# 2. Gráficos UMAP TF-IDF com cor por ANO
# ===================================
cat("\n=== GERANDO GRÁFICO UMAP (TF-IDF) COM COR POR ANO ===\n")

# Texto para hover no gráfico TF-IDF (com Ano)
hover_text_umap_tfidf_ano <- paste(
  "Música:", df_umap_tfidf$Musica,
  "<br>Artista:", ifelse(is.na(df_umap_tfidf$Artista), "N/A", df_umap_tfidf$Artista),
  "<br>Ano:", ifelse(is.na(df_umap_tfidf$Ano), "N/A", df_umap_tfidf$Ano),
  "<br>Total de palavras:", ifelse(is.na(df_umap_tfidf$NumPalavras), "N/A", df_umap_tfidf$NumPalavras)
)

grafico_umap_tfidf_ano <- plot_ly(
  data = df_umap_tfidf,
  x = ~UMAP1,
  y = ~UMAP2,
  text = hover_text_umap_tfidf_ano,
  type = 'scatter',
  mode = 'markers',
  color = ~Ano, # Colore por Ano
  marker = list(
    size = 8,
    opacity = 0.7,
    line = list(width = 1, color = "white"),
    colorscale = scale_plasma, # Usando a paleta 'plasma'
    cmin = min(df_umap_tfidf$Ano, na.rm = TRUE),
    cmax = max(df_umap_tfidf$Ano, na.rm = TRUE),
    colorbar = list(title = "Ano")
  ),
  hovertemplate = paste(
    "%{text}",
    "<extra></extra>"
  )
) %>%
  layout(
    title = list(
      text = "UMAP: Músicas (TF-IDF) - Coloração por Ano",
      font = list(size = 18, color = "#2c3e50")
    ),
    xaxis = list(title = "UMAP Componente 1"),
    yaxis = list(title = "UMAP Componente 2"),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest",
    font = list(family = "Arial, sans-serif")
  )

print(grafico_umap_tfidf_ano)
saveWidget(widget = grafico_umap_tfidf_ano, file = "umap_tfidf_cor_por_ano.html", selfcontained = TRUE)
cat("Gráfico UMAP TF-IDF colorido por Ano salvo em: umap_tfidf_cor_por_ano.html\n")


#==================================================================================================================

# ===================================
# 3. Gráficos UMAP TF com cor por NÚMERO DE PALAVRAS
# ===================================
cat("\n=== GERANDO GRÁFICO UMAP (TF) COM COR POR NÚMERO DE PALAVRAS ===\n")

# Texto para hover no gráfico TF (com NumPalavras)
hover_text_umap_tf_palavras <- paste(
  "Música:", df_umap_tf$Musica,
  "<br>Artista:", ifelse(is.na(df_umap_tf$Artista), "N/A", df_umap_tf$Artista),
  "<br>Ano:", ifelse(is.na(df_umap_tf$Ano), "N/A", df_umap_tf$Ano),
  "<br>Total de palavras:", ifelse(is.na(df_umap_tf$NumPalavras), "N/A", df_umap_tf$NumPalavras)
)

grafico_umap_tf_palavras <- plot_ly(
  data = df_umap_tf,
  x = ~UMAP1,
  y = ~UMAP2,
  text = hover_text_umap_tf_palavras,
  type = 'scatter',
  mode = 'markers',
  color = ~NumPalavras, # Colore por NumPalavras
  colors = "Reds", # <--- AQUI: Usando a paleta "Reds" diretamente
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
      text = "UMAP: Músicas (TF) - Coloração por Número de Palavras",
      font = list(size = 18, color = "#2c3e50")
    ),
    xaxis = list(title = "UMAP Componente 1"),
    yaxis = list(title = "UMAP Componente 2"),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest",
    font = list(family = "Arial, sans-serif")
  )

print(grafico_umap_tf_palavras)
saveWidget(widget = grafico_umap_tf_palavras, file = "umap_tf_cor_por_palavras.html", selfcontained = TRUE)
cat("Gráfico UMAP TF colorido por Número de Palavras salvo em: umap_tf_cor_por_palavras.html\n")

# ===================================
# 4. Gráficos UMAP TF-IDF com cor por NÚMERO DE PALAVRAS
# ===================================
cat("\n=== GERANDO GRÁFICO UMAP (TF-IDF) COM COR POR NÚMERO DE PALAVRAS ===\n")

# Texto para hover no gráfico TF-IDF (com NumPalavras)
hover_text_umap_tfidf_palavras <- paste(
  "Música:", df_umap_tfidf$Musica,
  "<br>Artista:", ifelse(is.na(df_umap_tfidf$Artista), "N/A", df_umap_tfidf$Artista),
  "<br>Ano:", ifelse(is.na(df_umap_tfidf$Ano), "N/A", df_umap_tfidf$Ano),
  "<br>Total de palavras:", ifelse(is.na(df_umap_tfidf$NumPalavras), "N/A", df_umap_tfidf$NumPalavras)
)

grafico_umap_tfidf_palavras <- plot_ly(
  data = df_umap_tfidf,
  x = ~UMAP1,
  y = ~UMAP2,
  text = hover_text_umap_tfidf_palavras,
  type = 'scatter',
  mode = 'markers',
  color = ~NumPalavras, # Colore por NumPalavras
  colors = "Reds", # <--- AQUI: Usando a paleta "Reds" diretamente
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
      text = "UMAP: Músicas (TF-IDF) - Coloração por Número de Palavras",
      font = list(size = 18, color = "#2c3e50")
    ),
    xaxis = list(title = "UMAP Componente 1"),
    yaxis = list(title = "UMAP Componente 2"),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest",
    font = list(family = "Arial, sans-serif")
  )

print(grafico_umap_tfidf_palavras)
saveWidget(widget = grafico_umap_tfidf_palavras, file = "umap_tfidf_cor_por_palavras.html", selfcontained = TRUE)
cat("Gráfico UMAP TF-IDF colorido por Número de Palavras salvo em: umap_tfidf_cor_por_palavras.html\n")
