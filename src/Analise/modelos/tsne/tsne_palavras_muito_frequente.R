# ===================================
# ANÁLISE t-SNE COM PALAVRAS MUITO FREQUENTES (>20 OCORRÊNCIAS)
# ===================================

# Carregar pacotes necessários
library(Rtsne)      # Para t-SNE
library(plotly)     # Para gráficos interativos
library(htmlwidgets) # Para salvar gráficos HTML

# ===================================
# 1. FILTRAR MATRIZ POR PALAVRAS MUITO FREQUENTES
# ===================================

# Calcular frequência total de cada palavra (assumindo que matriz_tf_t já existe)
frequencia_palavras <- colSums(matriz_tf_t)

# Filtrar palavras que aparecem mais de 20 vezes
palavras_muito_frequentes <- frequencia_palavras[frequencia_palavras > 20]

# Criar nova matriz TF com apenas palavras muito frequentes
nomes_colunas_frequentes <- names(palavras_muito_frequentes)
matriz_tf_muito_frequente <- matriz_tf_t[, nomes_colunas_frequentes]

# Exibir informações da filtragem
cat("=== FILTRAGEM POR PALAVRAS MUITO FREQUENTES (>20 OCORRÊNCIAS) ===\n")
cat("Matriz TF original:", nrow(matriz_tf_t), "músicas x", ncol(matriz_tf_t), "palavras\n")
cat("Matriz TF filtrada:", nrow(matriz_tf_muito_frequente), "x", ncol(matriz_tf_muito_frequente), "palavras\n")
cat("Matriz TF-IDF filtrada:", nrow(matriz_tfidf), "x", ncol(matriz_tfidf), "palavras\n")
cat("Número de palavras muito frequentes:", length(palavras_muito_frequentes), "\n")
cat("Percentual de palavras mantidas:", round(100 * ncol(matriz_tf_muito_frequente) / ncol(matriz_tf_t), 2), "%\n\n")

# Mostrar as 15 palavras mais frequentes
top_15_palavras <- sort(palavras_muito_frequentes, decreasing = TRUE)[1:15]
cat("=== TOP 15 PALAVRAS MAIS FREQUENTES ===\n")
print(top_15_palavras)

# ===================================
# 2. APLICAR t-SNE NA MATRIZ FILTRADA
# ===================================

cat("\n=== INICIANDO CÁLCULO t-SNE ===\n")
cat("Isso pode levar alguns minutos...\n")

# Configurar semente para reprodutibilidade
set.seed(42)

# Aplicar t-SNE
tsne_muito_frequente <- Rtsne(
  X = matriz_tf_muito_frequente,
  dims = 2,                    # 2 dimensões para visualização
  perplexity = 10,            # Ajuste conforme necessário
  theta = 0.5,                # Velocidade vs precisão
  pca = TRUE,                 # Aplicar PCA antes do t-SNE
  max_iter = 1000,            # Número máximo de iterações
  is_distance = FALSE,        # Matriz não é de distâncias
  check_duplicates = FALSE    # Ignorar duplicatas
)

cat("Cálculo t-SNE concluído!\n")
cat("Stress final:", tsne_muito_frequente$costs[length(tsne_muito_frequente$costs)], "\n\n")

# ===================================
# 3. FUNÇÃO PARA CRIAR GRÁFICOS INTERATIVOS
# ===================================

criar_grafico_tsne_interativo <- function(tsne_resultado, titulo, color_by = "Ano") {
  
  # Criar dataframe com coordenadas t-SNE e metadados
  df_tsne <- data.frame(
    TSNE1 = tsne_resultado$Y[, 1],
    TSNE2 = tsne_resultado$Y[, 2],
    Musica = nomes_musicas,
    Artista = artistas,
    Ano = anos,
    NumPalavras = num_palavras_tf
  )
  
  # Definir variável de cor e configurações
  if (color_by == "Ano") {
    color_var <- df_tsne$Ano
    color_title <- "Ano"
    colorscale <- list(
      c(0, "#F7FBFF"),    # Azul claro
      c(0.5, "#6BAED6"),  # Azul médio
      c(1, "#08306B")     # Azul escuro
    )
  } else if (color_by == "NumPalavras") {
    color_var <- df_tsne$NumPalavras
    color_title <- "Total de<br>Palavras"
    colorscale <- list(
      c(0, "#FFEDA0"),    # Amarelo claro
      c(0.5, "#FD8D3C"),  # Laranja
      c(1, "#BD0026")     # Vermelho escuro
    )
  }
  
  # Criar texto para hover
  hover_text <- paste(
    "Música:", df_tsne$Musica,
    "<br>Artista:", ifelse(is.na(df_tsne$Artista), "N/A", df_tsne$Artista),
    "<br>Ano:", ifelse(is.na(df_tsne$Ano), "N/A", df_tsne$Ano),
    "<br>Total de palavras:", ifelse(is.na(df_tsne$NumPalavras), "N/A", df_tsne$NumPalavras)
  )
  
  # Criar gráfico plotly
  grafico <- plot_ly(
    data = df_tsne,
    x = ~TSNE1,
    y = ~TSNE2,
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
        text = titulo,
        font = list(size = 18, color = "#2c3e50"),
        x = 0.5  # Centralizar título
      ),
      xaxis = list(
        title = "t-SNE Componente 1",
        titlefont = list(size = 14),
        showgrid = TRUE,
        gridcolor = "#ecf0f1",
        zeroline = FALSE
      ),
      yaxis = list(
        title = "t-SNE Componente 2", 
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
# 4. GERAR GRÁFICOS INTERATIVOS
# ===================================

cat("=== GERANDO GRÁFICOS INTERATIVOS ===\n")

# Gráfico colorido por Ano
grafico_tsne_ano <- criar_grafico_tsne_interativo(
  tsne_resultado = tsne_muito_frequente,
  titulo = "t-SNE: Músicas com Base em Palavras Muito Frequentes — Colorido por Ano",
  color_by = "Ano"
)

# Gráfico colorido por Número de Palavras
grafico_tsne_palavras <- criar_grafico_tsne_interativo(
  tsne_resultado = tsne_muito_frequente,
  titulo = "t-SNE: Músicas com Base em Palavras Muito Frequentes — Colorido por Total de Palavras",
  color_by = "NumPalavras"
)

# Exibir gráficos
print(grafico_tsne_ano)
print(grafico_tsne_palavras)

cat("Gráficos t-SNE gerados com sucesso!\n\n")

# ===================================
# 5. SALVAR GRÁFICOS (OPCIONAL - DESCOMENTE PARA USAR)
# ===================================

# Definir nomes dos arquivos
arquivo_tsne_ano <- "tsne_tf_palavras_muito_frequentes_ano.html"
arquivo_tsne_palavras <- "tsne_tf_palavras_muito_frequentes_total_palavras.html"

# Salvar gráfico colorido por Ano
saveWidget(
 widget = grafico_tsne_ano,
 file = arquivo_tsne_ano,
 selfcontained = TRUE,
 title = "t-SNE Músicas por Ano"
)

cat("Gráfico t-SNE (colorido por Ano) salvo em:", arquivo_tsne_ano, "\n")

# Salvar gráfico colorido por Total de Palavras
saveWidget(
 widget = grafico_tsne_palavras,
 file = arquivo_tsne_palavras,
 selfcontained = TRUE,
 title = "t-SNE Músicas por Total de Palavras"
)

cat("Gráfico t-SNE (colorido por Total de Palavras) salvo em:", arquivo_tsne_palavras, "\n")
cat("\nAmbos os gráficos foram salvos como arquivos HTML interativos.\n")
cat("Você pode abri-los em qualquer navegador web para explorar os dados.\n")

# ===================================
# 6. ESTATÍSTICAS FINAIS
# ===================================

cat("=== RESUMO DA ANÁLISE ===\n")
cat("Dimensões da matriz final:", nrow(matriz_tf_muito_frequente), "x", ncol(matriz_tf_muito_frequente), "\n")
cat("Número de músicas analisadas:", nrow(matriz_tf_muito_frequente), "\n")
cat("Número de palavras muito frequentes:", ncol(matriz_tf_muito_frequente), "\n")
cat("Range t-SNE Componente 1:", round(range(tsne_muito_frequente$Y[,1]), 2), "\n")
cat("Range t-SNE Componente 2:", round(range(tsne_muito_frequente$Y[,2]), 2), "\n")
cat("\nAnálise t-SNE concluída com sucesso!\n")

#=========================================================================
#=========================================================================
#=========================================================================
#=========================================================================


# ===================================
# 2. CALCULAR TF-IDF NA MATRIZ FILTRADA
# ===================================

cat("\n=== CALCULANDO PESOS TF-IDF ===\n")

# Função para calcular TF-IDF
calcular_tfidf <- function(matriz_tf) {
  # Número total de documentos (músicas)
  n_documentos <- nrow(matriz_tf)
  
  # Calcular IDF para cada palavra (coluna)
  # IDF = log(n_documentos / número de documentos que contêm a palavra)
  documentos_com_palavra <- colSums(matriz_tf > 0)  # Conta documentos onde palavra aparece
  idf_weights <- log(n_documentos / documentos_com_palavra)
  
  # Calcular TF-IDF multiplicando TF por IDF
  # Cada linha (documento) é multiplicada pelos pesos IDF correspondentes
  matriz_tfidf <- sweep(matriz_tf, 2, idf_weights, "*")
  
  return(list(
    matriz_tfidf = matriz_tfidf,
    idf_weights = idf_weights,
    documentos_com_palavra = documentos_com_palavra
  ))
}

# Aplicar cálculo TF-IDF na matriz já filtrada
resultado_tfidf <- calcular_tfidf(matriz_tf_muito_frequente)
matriz_tfidf <- resultado_tfidf$matriz_tfidf
idf_weights <- resultado_tfidf$idf_weights
resultado_tfidf$documentos_com_palavra

# Exibir estatísticas do TF-IDF
cat("Dimensões da matriz TF-IDF:", nrow(matriz_tfidf), "x", ncol(matriz_tfidf), "\n")
cat("Estatísticas dos pesos IDF:\n")
print(summary(idf_weights))
cat("\nEstatísticas dos valores TF-IDF:\n")
print(summary(as.vector(matriz_tfidf)))

# Mostrar palavras com maiores e menores pesos IDF
cat("\n=== PALAVRAS COM MAIORES PESOS IDF (mais discriminativas) ===\n")
top_idf <- sort(idf_weights, decreasing = TRUE)[1:10]
print(round(top_idf, 3))

cat("\n=== PALAVRAS COM MENORES PESOS IDF (mais comuns) ===\n")
bottom_idf <- sort(idf_weights, decreasing = FALSE)[1:10]
print(round(bottom_idf, 3))

# ===================================
# 3. APLICAR t-SNE NA MATRIZ TF-IDF
# ===================================

cat("\n=== INICIANDO CÁLCULO t-SNE COM TF-IDF ===\n")
cat("Isso pode levar alguns minutos...\n")

# Configurar semente para reprodutibilidade
set.seed(42)

# Aplicar t-SNE na matriz TF-IDF
tsne_tfidf <- Rtsne(
  X = matriz_tfidf,
  dims = 2,                    # 2 dimensões para visualização
  perplexity = 50,            # Ajuste conforme necessário
  theta = 0.5,                # Velocidade vs precisão
  pca = TRUE,                 # Aplicar PCA antes do t-SNE
  max_iter = 1000,            # Número máximo de iterações
  is_distance = FALSE,        # Matriz não é de distâncias
  check_duplicates = FALSE    # Ignorar duplicatas
)

cat("Cálculo t-SNE concluído!\n")
cat("Stress final:", tsne_tfidf$costs[length(tsne_tfidf$costs)], "\n\n")

# ===================================
# 4. FUNÇÃO PARA CRIAR GRÁFICOS INTERATIVOS
# ===================================

criar_grafico_tsne_tfidf <- function(tsne_resultado, titulo, color_by = "Ano") {
  
  # Criar dataframe com coordenadas t-SNE e metadados
  df_tsne <- data.frame(
    TSNE1 = tsne_resultado$Y[, 1],
    TSNE2 = tsne_resultado$Y[, 2],
    Musica = nomes_musicas,
    Artista = artistas,
    Ano = anos,
    NumPalavras = num_palavras_tf
  )
  
  # Definir variável de cor e configurações
  if (color_by == "Ano") {
    color_var <- df_tsne$Ano
    color_title <- "Ano"
    colorscale <- list(
      c(0, "#F7FBFF"),    # Azul claro
      c(0.5, "#6BAED6"),  # Azul médio
      c(1, "#08306B")     # Azul escuro
    )
  } else if (color_by == "NumPalavras") {
    color_var <- df_tsne$NumPalavras
    color_title <- "Total de<br>Palavras"
    colorscale <- list(
      c(0, "#FFEDA0"),    # Amarelo claro
      c(0.5, "#FD8D3C"),  # Laranja
      c(1, "#BD0026")     # Vermelho escuro
    )
  }
  
  # Criar texto para hover
  hover_text <- paste(
    "Música:", df_tsne$Musica,
    "<br>Artista:", ifelse(is.na(df_tsne$Artista), "N/A", df_tsne$Artista),
    "<br>Ano:", ifelse(is.na(df_tsne$Ano), "N/A", df_tsne$Ano),
    "<br>Total de palavras:", ifelse(is.na(df_tsne$NumPalavras), "N/A", df_tsne$NumPalavras)
  )
  
  # Criar gráfico plotly
  grafico <- plot_ly(
    data = df_tsne,
    x = ~TSNE1,
    y = ~TSNE2,
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
        text = titulo,
        font = list(size = 18, color = "#2c3e50"),
        x = 0.5  # Centralizar título
      ),
      xaxis = list(
        title = "t-SNE Componente 1",
        titlefont = list(size = 14),
        showgrid = TRUE,
        gridcolor = "#ecf0f1",
        zeroline = FALSE
      ),
      yaxis = list(
        title = "t-SNE Componente 2", 
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
# 5. GERAR GRÁFICOS INTERATIVOS
# ===================================

cat("=== GERANDO GRÁFICOS INTERATIVOS TF-IDF ===\n")

# Gráfico colorido por Ano
grafico_tfidf_ano <- criar_grafico_tsne_tfidf(
  tsne_resultado = tsne_tfidf,
  titulo = "t-SNE: Músicas com Representação TF-IDF (Palavras >20 ocorrências) — Colorido por Ano",
  color_by = "Ano"
)

# Gráfico colorido por Número de Palavras
grafico_tfidf_palavras <- criar_grafico_tsne_tfidf(
  tsne_resultado = tsne_tfidf,
  titulo = "t-SNE: Músicas com Representação TF-IDF (Palavras >20 ocorrências) — Colorido por Total de Palavras",
  color_by = "NumPalavras"
)

# Exibir gráficos
print(grafico_tfidf_ano)
print(grafico_tfidf_palavras)

cat("Gráficos t-SNE com TF-IDF gerados com sucesso!\n\n")

# ===================================
# 6. SALVAR GRÁFICOS (OPCIONAL - DESCOMENTE PARA USAR)
# ===================================

# Definir nomes dos arquivos
arquivo_tfidf_ano <- "tsne_tfidf_palavras_muito_frequentes_ano.html"
arquivo_tfidf_palavras <- "tsne_tfidf_palavras_muito_frequentes_total_palavras.html"

# Salvar gráfico colorido por Ano
saveWidget(
 widget = grafico_tfidf_ano,
 file = arquivo_tfidf_ano,
 selfcontained = TRUE,
 title = "t-SNE TF-IDF Músicas por Ano"
)

cat("Gráfico t-SNE TF-IDF (colorido por Ano) salvo em:", arquivo_tfidf_ano, "\n")

# Salvar gráfico colorido por Total de Palavras
saveWidget(
 widget = grafico_tfidf_palavras,
 file = arquivo_tfidf_palavras,
 selfcontained = TRUE,
 title = "t-SNE TF-IDF Músicas por Total de Palavras"
)

cat("Gráfico t-SNE TF-IDF (colorido por Total de Palavras) salvo em:", arquivo_tfidf_palavras, "\n")
cat("\nAmbos os gráficos TF-IDF foram salvos como arquivos HTML interativos.\n")
cat("Você pode abri-los em qualquer navegador web para explorar os dados.\n")
 ===================================
# 7. COMPARAÇÃO E ESTATÍSTICAS FINAIS
# ===================================

cat("=== RESUMO DA ANÁLISE TF-IDF ===\n")
cat("Dimensões da matriz TF original filtrada:", nrow(matriz_tf_filtrada), "x", ncol(matriz_tf_filtrada), "\n")
cat("Dimensões da matriz TF-IDF:", nrow(matriz_tfidf), "x", ncol(matriz_tfidf), "\n")
cat("Número de músicas analisadas:", nrow(matriz_tfidf), "\n")
cat("Número de palavras muito frequentes:", ncol(matriz_tfidf), "\n")
cat("Range dos pesos IDF:", round(range(idf_weights), 3), "\n")
cat("Range t-SNE Componente 1:", round(range(tsne_tfidf$Y[,1]), 2), "\n")
cat("Range t-SNE Componente 2:", round(range(tsne_tfidf$Y[,2]), 2), "\n")

# Comparar densidade de valores não-zero
sparsity_tf <- sum(matriz_tf_filtrada == 0) / (nrow(matriz_tf_filtrada) * ncol(matriz_tf_filtrada))
sparsity_tfidf <- sum(matriz_tfidf == 0) / (nrow(matriz_tfidf) * ncol(matriz_tfidf))

cat("\nSparsity (% de zeros):\n")
cat("  Matriz TF:", round(sparsity_tf * 100, 2), "%\n")
cat("  Matriz TF-IDF:", round(sparsity_tfidf * 100, 2), "%\n")
cat("\nAnálise t-SNE com TF-IDF concluída com sucesso!\n")
cat("\nIMPORTANTE: TF-IDF dá mais peso a palavras distintivas e menos peso a palavras comuns,\n")
cat("resultando em agrupamentos potencialmente diferentes do TF simples.\n")

