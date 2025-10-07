# ===================================
# ANÁLISE PCA COM REPRESENTAÇÕES TF E TF-IDF
# Para palavras que aparecem mais de 20 vezes
# ===================================

# Carregar pacotes necessários
library(plotly)     # Para gráficos interativos
library(htmlwidgets) # Para salvar gráficos HTML

# ===================================
# 1. CALCULAR PCA PARA A MATRIZ TF FILTRADA
# ===================================

cat("=== CALCULANDO PCA PARA REPRESENTAÇÃO TF ===\n")

# Aplicar PCA na matriz TF filtrada (palavras > 20 ocorrências)
pca_tf_filtrado <- prcomp(matriz_tf_muito_frequente, center = TRUE, scale. = TRUE)

# Exibir informações do PCA TF
variancia_explicada_tf <- summary(pca_tf_filtrado)$importance[2, 1:5] * 100
cat("Variância explicada pelas primeiras 5 componentes (TF):\n")
for(i in 1:5) {
  cat(sprintf("  PC%d: %.2f%%\n", i, variancia_explicada_tf[i]))
}
cat(sprintf("Total das duas primeiras componentes (TF): %.2f%%\n", sum(variancia_explicada_tf[1:2])))

# ===================================
# 2. CALCULAR PCA PARA A MATRIZ TF-IDF
# ===================================

cat("\n=== CALCULANDO PCA PARA REPRESENTAÇÃO TF-IDF ===\n")

# Aplicar PCA na matriz TF-IDF (já calculada no código anterior)
pca_tfidf <- prcomp(matriz_tfidf, center = TRUE, scale. = TRUE)

# Exibir informações do PCA TF-IDF
variancia_explicada_tfidf <- summary(pca_tfidf)$importance[2, 1:5] * 100
cat("Variância explicada pelas primeiras 5 componentes (TF-IDF):\n")
for(i in 1:5) {
  cat(sprintf("  PC%d: %.2f%%\n", i, variancia_explicada_tfidf[i]))
}
cat(sprintf("Total das duas primeiras componentes (TF-IDF): %.2f%%\n", sum(variancia_explicada_tfidf[1:2])))

# ===================================
# 3. FUNÇÃO PARA CRIAR GRÁFICOS PCA INTERATIVOS
# ===================================

criar_grafico_pca_interativo <- function(pca_resultado, titulo, color_by = "Ano") {
  # Extrair coordenadas das duas primeiras componentes
  coordenadas <- data.frame(
    PC1 = pca_resultado$x[,1],
    PC2 = pca_resultado$x[,2],
    Musica = nomes_musicas,
    Artista = artistas,
    Ano = anos,
    NumPalavras = num_palavras_tf
  )
  
  # Definir variável de cor e configurações
  if (color_by == "Ano") {
    color_var <- coordenadas$Ano
    color_title <- "Ano"
    colorscale <- list(
      c(0, "#F7FBFF"),    # Azul claro
      c(0.5, "#6BAED6"),  # Azul médio
      c(1, "#08306B")     # Azul escuro
    )
  } else if (color_by == "NumPalavras") {
    color_var <- coordenadas$NumPalavras
    color_title <- "Total de<br>Palavras"
    colorscale <- list(
      c(0, "#FFEDA0"),    # Amarelo claro
      c(0.5, "#FD8D3C"),  # Laranja
      c(1, "#BD0026")     # Vermelho escuro
    )
  }
  
  # Calcular variância explicada para o título
  var_explicada <- summary(pca_resultado)$importance[2, 1:2] * 100
  
  # Criar texto para hover
  hover_text <- paste(
    "Música:", coordenadas$Musica,
    "<br>Artista:", ifelse(is.na(coordenadas$Artista), "N/A", coordenadas$Artista),
    "<br>Ano:", ifelse(is.na(coordenadas$Ano), "N/A", coordenadas$Ano),
    "<br>Total de palavras:", ifelse(is.na(coordenadas$NumPalavras), "N/A", coordenadas$NumPalavras)
  )
  
  # Criar gráfico plotly
  grafico <- plot_ly(
    data = coordenadas,
    x = ~PC1,
    y = ~PC2,
    text = hover_text,
    type = 'scatter',
    mode = 'markers',
    marker = list(
      size = 10,
      color = color_var,
      colorscale = colorscale,
      opacity = 0.8,
      line = list(width = 1, color = "white"),
      colorbar = list(
        title = list(text = color_title, font = list(size = 14)),
        titleside = "right"
      )
    ),
    hovertemplate = paste(
      "%{text}",
      "<extra></extra>"  # Remove caixa padrão do plotly
    )
  ) %>%
    layout(
      title = list(
        text = paste0(titulo, "<br><sub>PC1: ", round(var_explicada[1], 1),
                      "% | PC2: ", round(var_explicada[2], 1), "%</sub>"),
        font = list(size = 18, color = "#2c3e50"),
        x = 0.5  # Centralizar título
      ),
      xaxis = list(
        title = paste0("PC1 (", round(var_explicada[1], 1), "%)"),
        titlefont = list(size = 14),
        showgrid = TRUE,
        gridcolor = "#ecf0f1",
        zeroline = FALSE
      ),
      yaxis = list(
        title = paste0("PC2 (", round(var_explicada[2], 1), "%)"),
        titlefont = list(size = 14),
        showgrid = TRUE,
        gridcolor = "#ecf0f1",
        zeroline = FALSE
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      hovermode = "closest",
      font = list(family = "Arial, sans-serif")
    )
  
  return(grafico)
}

# ===================================
# 4. GERAR GRÁFICOS PCA PARA REPRESENTAÇÃO TF
# ===================================

cat("\n=== GERANDO GRÁFICOS PCA PARA REPRESENTAÇÃO TF ===\n")

# Gráfico TF colorido por Ano
grafico_pca_tf_ano <- criar_grafico_pca_interativo(
  pca_resultado = pca_tf_filtrado,
  titulo = "PCA: Representação TF (Palavras >20 ocorrências) — Colorido por Ano",
  color_by = "Ano"
)

# Gráfico TF colorido por Número de Palavras
grafico_pca_tf_palavras <- criar_grafico_pca_interativo(
  pca_resultado = pca_tf_filtrado,
  titulo = "PCA: Representação TF (Palavras >20 ocorrências) — Colorido por Total de Palavras",
  color_by = "NumPalavras"
)

# Aplicar zoom nos gráficos TF (xlim = c(-5, 5), ylim = c(-5, 5))
grafico_pca_tf_ano_zoom <- grafico_pca_tf_ano %>%
  layout(
    xaxis = list(range = c(-5, 5)),
    yaxis = list(range = c(-5, 5))
  )

grafico_pca_tf_palavras_zoom <- grafico_pca_tf_palavras %>%
  layout(
    xaxis = list(range = c(-5, 5)),
    yaxis = list(range = c(-5, 5))
  )

# Exibir gráficos TF
print(grafico_pca_tf_ano_zoom)
print(grafico_pca_tf_palavras_zoom)

# ===================================
# 5. GERAR GRÁFICOS PCA PARA REPRESENTAÇÃO TF-IDF
# ===================================

cat("\n=== GERANDO GRÁFICOS PCA PARA REPRESENTAÇÃO TF-IDF ===\n")

# Gráfico TF-IDF colorido por Ano
grafico_pca_tfidf_ano <- criar_grafico_pca_interativo(
  pca_resultado = pca_tfidf,
  titulo = "PCA: Representação TF-IDF (Palavras >20 ocorrências) — Colorido por Ano",
  color_by = "Ano"
)

# Gráfico TF-IDF colorido por Número de Palavras
grafico_pca_tfidf_palavras <- criar_grafico_pca_interativo(
  pca_resultado = pca_tfidf,
  titulo = "PCA: Representação TF-IDF (Palavras >20 ocorrências) — Colorido por Total de Palavras",
  color_by = "NumPalavras"
)

# Aplicar zoom nos gráficos TF-IDF (xlim = c(-5, 5), ylim = c(-5, 5))
grafico_pca_tfidf_ano_zoom <- grafico_pca_tfidf_ano %>%
  layout(
    xaxis = list(range = c(-5, 5)),
    yaxis = list(range = c(-5, 5))
  )

grafico_pca_tfidf_palavras_zoom <- grafico_pca_tfidf_palavras %>%
  layout(
    xaxis = list(range = c(-5, 5)),
    yaxis = list(range = c(-5, 5))
  )

# Exibir gráficos TF-IDF
print(grafico_pca_tfidf_ano_zoom)
print(grafico_pca_tfidf_palavras_zoom)

# ===================================
# 6. SALVAR GRÁFICOS PCA
# ===================================

cat("\n=== SALVANDO GRÁFICOS PCA ===\n")

# Definir nomes dos arquivos
arquivo_pca_tf_ano <- "pca_tf_muito_frequentes_ano.html"
arquivo_pca_tf_palavras <- "pca_tf_muito_frequentes_palavras.html"
arquivo_pca_tfidf_ano <- "pca_tfidf_muito_frequentes_ano.html"
arquivo_pca_tfidf_palavras <- "pca_tfidf_muito_frequentes_palavras.html"

# Salvar gráficos TF
saveWidget(
  widget = grafico_pca_tf_ano_zoom,
  file = arquivo_pca_tf_ano,
  selfcontained = TRUE,
  title = "PCA TF por Ano"
)

saveWidget(
  widget = grafico_pca_tf_palavras_zoom,
  file = arquivo_pca_tf_palavras,
  selfcontained = TRUE,
  title = "PCA TF por Total de Palavras"
)

# Salvar gráficos TF-IDF
saveWidget(
  widget = grafico_pca_tfidf_ano_zoom,
  file = arquivo_pca_tfidf_ano,
  selfcontained = TRUE,
  title = "PCA TF-IDF por Ano"
)

saveWidget(
  widget = grafico_pca_tfidf_palavras_zoom,
  file = arquivo_pca_tfidf_palavras,
  selfcontained = TRUE,
  title = "PCA TF-IDF por Total de Palavras"
)

# Exibir confirmação dos arquivos salvos
cat("Gráficos PCA salvos com sucesso:\n")
cat("  TF por Ano:", arquivo_pca_tf_ano, "\n")
cat("  TF por Total de Palavras:", arquivo_pca_tf_palavras, "\n")
cat("  TF-IDF por Ano:", arquivo_pca_tfidf_ano, "\n")
cat("  TF-IDF por Total de Palavras:", arquivo_pca_tfidf_palavras, "\n")

# ===================================
# 7. ANÁLISE COMPARATIVA E ESTATÍSTICAS
# ===================================

cat("\n=== ANÁLISE COMPARATIVA PCA ===\n")

# Comparar variância explicada
cat("Comparação de Variância Explicada (primeiras 2 componentes):\n")
cat(sprintf("  TF: %.2f%% (PC1: %.2f%%, PC2: %.2f%%)\n", 
            sum(variancia_explicada_tf[1:2]), variancia_explicada_tf[1], variancia_explicada_tf[2]))
cat(sprintf("  TF-IDF: %.2f%% (PC1: %.2f%%, PC2: %.2f%%)\n", 
            sum(variancia_explicada_tfidf[1:2]), variancia_explicada_tfidf[1], variancia_explicada_tfidf[2]))

# Analisar contribuições das variáveis (loadings)
cat("\n=== TOP 10 PALAVRAS MAIS IMPORTANTES ===\n")

# Para TF - PC1
loadings_tf_pc1 <- abs(pca_tf_filtrado$rotation[,1])
top_palavras_tf_pc1 <- names(sort(loadings_tf_pc1, decreasing = TRUE)[1:10])
cat("TF - PC1:\n", paste(top_palavras_tf_pc1, collapse = ", "), "\n")

# Para TF - PC2
loadings_tf_pc2 <- abs(pca_tf_filtrado$rotation[,2])
top_palavras_tf_pc2 <- names(sort(loadings_tf_pc2, decreasing = TRUE)[1:10])
cat("TF - PC2:\n", paste(top_palavras_tf_pc2, collapse = ", "), "\n")

# Para TF-IDF - PC1
loadings_tfidf_pc1 <- abs(pca_tfidf$rotation[,1])
top_palavras_tfidf_pc1 <- names(sort(loadings_tfidf_pc1, decreasing = TRUE)[1:10])
cat("TF-IDF - PC1:\n", paste(top_palavras_tfidf_pc1, collapse = ", "), "\n")

# Para TF-IDF - PC2
loadings_tfidf_pc2 <- abs(pca_tfidf$rotation[,2])
top_palavras_tfidf_pc2 <- names(sort(loadings_tfidf_pc2, decreasing = TRUE)[1:10])
cat("TF-IDF - PC2:\n", paste(top_palavras_tfidf_pc2, collapse = ", "), "\n")

# ===================================
# 8. RESUMO FINAL
# ===================================

cat("\n=== RESUMO FINAL DA ANÁLISE PCA ===\n")
cat("Dimensões analisadas:\n")
cat("  Número de músicas:", nrow(matriz_tf_muito_frequente), "\n")
cat("  Número de palavras (>20 ocorrências):", ncol(matriz_tf_muito_frequente), "\n")
cat("\nArquivos gerados:\n")
cat("  - 4 gráficos PCA interativos em HTML\n")
cat("  - Análise comparativa entre TF e TF-IDF\n")
cat("  - Identificação das palavras mais importantes por componente\n")
cat("\nTodos os gráficos incluem zoom (-5 a 5) e são totalmente interativos.\n")
cat("Análise PCA concluída com sucesso!\n")