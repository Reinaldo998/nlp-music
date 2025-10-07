# Carregar pacotes necessários
library(dplyr)
library(tm)
library(slam)
library(FactoMineR) # Para PCA (prcomp)
library(factoextra) # Para fviz_eig
library(ggplot2)    # Base para gráficos (usado por factoextra)
library(plotly)     # Para gráficos interativos
library(htmlwidgets) # Para salvar gráficos plotly como HTML

set.seed(123) # Para resultados reprodutíveis

# =================================================================
# 1. PREPARAÇÃO DOS DADOS DE ENTRADA (USANDO SEU data_final_pt EXISTENTE)
# =================================================================
# ATENÇÃO: NÃO RECARREGUE OU REINICIALIZE 'data_final_pt' AQUI.
# ESTE CÓDIGO ASSUME QUE SEU 'data_final_pt' JÁ ESTÁ CARREGADO E CORRETO
# EM SUA SESSÃO R.
# Se você lê de um arquivo (CSV, RDS, etc.), certifique-se de que a linha de leitura
# DESSE ARQUIVO JÁ FOI EXECUTADA EM ALGUM PONTO ANTERIOR NA SUA SESSÃO.

# Exemplo: Se você carrega assim:
# data_final_pt <- read.csv("caminho/para/seus/dados/data_final_pt.csv", stringsAsFactors = FALSE)
# OU
# data_final_pt <- readRDS("caminho/para/seus/dados/data_final_pt.rds")
# ENTÃO, A LINHA ACIMA DEVE TER SIDO EXECUTADA ANTES DESTE BLOCO.
# =================================================================

# Extrair metadados das músicas do seu 'data_final_pt' existente
artistas <- data_final_pt$Artista.x
anos <- data_final_pt$Ano
nomes_musicas <- data_final_pt$Nome.x

# ----------------------------
# Função de pré-processamento de texto
# ----------------------------
limpar_corpus <- function(texto) {
  corpus <- VCorpus(VectorSource(texto)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("pt")) %>% # Assumes stopwords("pt") está disponível
    tm_map(stripWhitespace)
  return(corpus)
}

# ===================================
# 2. CRIAÇÃO DAS 3 REPRESENTAÇÕES DA LETRA DA MÚSICA
# ===================================

# -------------------------------
# 2.1. Term-Document Matrix (TF bruto)
# -------------------------------
corpus_tf <- limpar_corpus(data_final_pt$Letra)
tdm_tf <- TermDocumentMatrix(corpus_tf)
matriz_tf <- as.matrix(tdm_tf)
matriz_tf_t <- t(matriz_tf) # Transposta: músicas nas linhas, palavras nas colunas

# Calcular número total de palavras por música (soma das linhas da matriz TF)
num_palavras_tf <- rowSums(matriz_tf_t)

# -------------------------------
# 2.2. Matriz Binária (presença/ausência)
# -------------------------------
matriz_binaria <- matriz_tf
matriz_binaria[matriz_binaria > 0] <- 1
matriz_binaria_t <- t(matriz_binaria)

# -------------------------------
# 2.3. Term-Document Matrix com peso TF-IDF
# -------------------------------
corpus_tfidf <- limpar_corpus(data_final_pt$Letra)
tdm_tfidf <- TermDocumentMatrix(corpus_tfidf,
                                control = list(weighting = weightTfIdf))
matriz_tfidf <- as.matrix(tdm_tfidf)
matriz_tfidf_t <- t(matriz_tfidf)

# Remover termos (colunas) com variância zero para TF-IDF
# Isso é crucial para prcomp, que não lida bem com variância zero.
variancias <- apply(matriz_tfidf_t, 2, var)

# Adicionado um fallback caso todas as colunas tenham variância zero.
# Isso garante que a matriz não fique vazia e que o PCA possa ser calculado.
if(all(variancias == 0) || ncol(matriz_tfidf_t[ , variancias > 0]) == 0) {
  warning("Todas as palavras em TF-IDF possuem variância zero ou a matriz ficou vazia após o filtro. PCA para TF-IDF pode não ser significativo.")
  matriz_tfidf_filtrada <- matriz_tfidf_t # Usa a matriz original (sem filtro de variância)
} else {
  matriz_tfidf_filtrada <- matriz_tfidf_t[, variancias > 0]
}

# ===================================
# 3. APLICAÇÃO DO PCA NAS 3 REPRESENTAÇÕES
# ===================================

# PCA 1: TF
pca_tf <- prcomp(matriz_tf_t, scale. = TRUE)

# PCA 2: Matriz Binária
pca_bin <- prcomp(matriz_binaria_t, scale. = TRUE)

# PCA 3: TF-IDF
pca_tfidf <- prcomp(matriz_tfidf_filtrada, scale. = TRUE)

# ===================================
# 4. FUNÇÃO PARA CRIAR GRÁFICOS PLOTLY (MODIFICADA)
# ===================================
# Esta função foi ajustada para aceitar um parâmetro 'color_by'
# que controla a variável de coloração e remove a dependência de cos^2.

criar_grafico_pca_plotly <- function(pca_resultado, titulo, nomes_musicas, artistas, anos, num_palavras, color_by = "Ano") {
  # Extrair coordenadas das duas primeiras componentes
  n_obs <- nrow(pca_resultado$x)
  
  # Criar dataframe de coordenadas
  # Assume-se que os vetores de metadados já estão alinhados com as observações do PCA.
  # Se você estiver recebendo o erro "argumentos implicam em número de linhas distintos",
  # a causa ainda será que um ou mais desses vetores (nomes_musicas, artistas, anos, num_palavras)
  # não tem o mesmo número de linhas que pca_resultado$x.
  # Execute as verificações de `length()` e `nrow()` que sugeri anteriormente para depurar.
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
  
  
  # Calcular variância explicada pelas duas primeiras componentes
  var_explicada <- summary(pca_resultado)$importance[2, 1:2] * 100
  
  # Criar hover text (REMOVIDA A INFORMAÇÃO DO cos^2, conforme solicitado)
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
      color = color_var, # AGORA COLORIDO PELA VARIÁVEL ESCOLHIDA (Ano ou NumPalavras)
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
# 5. CRIAÇÃO E EXIBIÇÃO DOS GRÁFICOS PLOTLY (6 gráficos no total)
# ===================================

# --- Gráficos PCA TF ---
grafico_pca_tf_palavras <- criar_grafico_pca_plotly(
  pca_resultado = pca_tf,
  titulo = "PCA - Representação TF — Colorido por Total de Palavras",
  num_palavras = num_palavras_tf,
  nomes_musicas = nomes_musicas,
  artistas = artistas,
  anos = anos,
  color_by = "NumPalavras" # Especifica para colorir por número de palavras
)

grafico_pca_tf_anos <- criar_grafico_pca_plotly(
  pca_resultado = pca_tf,
  titulo = "PCA - Representação TF — Colorido por Ano",
  num_palavras = num_palavras_tf,
  nomes_musicas = nomes_musicas,
  artistas = artistas,
  anos = anos,
  color_by = "Ano" # Especifica para colorir por ano
)

# --- Gráficos PCA Binário ---
grafico_pca_bin_palavras <- criar_grafico_pca_plotly(
  pca_resultado = pca_bin,
  titulo = "PCA - Representação Binária — Colorido por Total de Palavras",
  num_palavras = num_palavras_tf, # Usando a mesma contagem de palavras como referência
  nomes_musicas = nomes_musicas,
  artistas = artistas,
  anos = anos,
  color_by = "NumPalavras"
)

grafico_pca_bin_anos <- criar_grafico_pca_plotly(
  pca_resultado = pca_bin,
  titulo = "PCA - Representação Binária — Colorido por Ano",
  num_palavras = num_palavras_tf, # Usando a mesma contagem de palavras como referência
  nomes_musicas = nomes_musicas,
  artistas = artistas,
  anos = anos,
  color_by = "Ano"
)

# --- Gráficos PCA TF-IDF ---
# IMPORTANT: Metadados (nomes_musicas, artistas, anos, num_palavras_tf)
# são passados completos aqui, pois o filtro de variância afetou COLUNAS (palavras),
# não LINHAS (músicas) do PCA.
grafico_pca_tfidf_palavras <- criar_grafico_pca_plotly(
  pca_resultado = pca_tfidf,
  titulo = "PCA - Representação TF-IDF — Colorido por Total de Palavras",
  num_palavras = num_palavras_tf,
  nomes_musicas = nomes_musicas,
  artistas = artistas,
  anos = anos,
  color_by = "NumPalavras"
)

grafico_pca_tfidf_anos <- criar_grafico_pca_plotly(
  pca_resultado = pca_tfidf,
  titulo = "PCA - Representação TF-IDF — Colorido por Ano",
  num_palavras = num_palavras_tf,
  nomes_musicas = nomes_musicas,
  artistas = artistas,
  anos = anos,
  color_by = "Ano"
)

# ===================================
# 6. EXIBIÇÃO DOS GRÁFICOS
# ===================================
cat("=== GRÁFICOS PCA TF - COLORIDO POR PALAVRAS ===\n")
print(grafico_pca_tf_palavras)

cat("=== GRÁFICOS PCA TF - COLORIDO POR ANO ===\n")
print(grafico_pca_tf_anos)

cat("=== GRÁFICOS PCA BINÁRIO - COLORIDO POR PALAVRAS ===\n")
print(grafico_pca_bin_palavras)

cat("=== GRÁFICOS PCA BINÁRIO - COLORIDO POR ANO ===\n")
print(grafico_pca_bin_anos)

cat("=== GRÁFICOS PCA TF-IDF - COLORIDO POR PALAVRAS ===\n")
print(grafico_pca_tfidf_palavras)

cat("=== GRÁFICOS PCA TF-IDF - COLORIDO POR ANO ===\n")
print(grafico_pca_tfidf_anos)

# ===================================
# 7. GRÁFICOS DE VARIÂNCIA EXPLICADA (Mantido)
# ===================================
cat("\n=== VARIÂNCIA EXPLICADA - TF ===\n")
print(fviz_eig(pca_tf, main = "PCA TF - Variância Explicada"))

cat("\n=== VARIÂNCIA EXPLICADA - BINÁRIO ===\n")
print(fviz_eig(pca_bin, main = "PCA Binário - Variância Explicada"))

cat("\n=== VARIÂNCIA EXPLICADA - TF-IDF ===\n")
print(fviz_eig(pca_tfidf, main = "PCA TF-IDF - Variância Explicada"))

# ===================================
# 8. SALVAMENTO DOS GRÁFICOS (OPCIONAL)
# Remova o comentário das linhas abaixo para salvar os gráficos
# ===================================
saveWidget(grafico_pca_tf_palavras, file = "pca_tf_palavras_plotly.html", selfcontained = TRUE)
saveWidget(grafico_pca_tf_anos, file = "pca_tf_anos_plotly.html", selfcontained = TRUE)
saveWidget(grafico_pca_bin_palavras, file = "pca_binario_palavras_plotly.html", selfcontained = TRUE)
saveWidget(grafico_pca_bin_anos, file = "pca_binario_anos_plotly.html", selfcontained = TRUE)
saveWidget(grafico_pca_tfidf_palavras, file = "pca_tfidf_palavras_plotly.html", selfcontained = TRUE)
saveWidget(grafico_pca_tfidf_anos, file = "pca_tfidf_anos_plotly.html", selfcontained = TRUE)
#
# cat("\nGráficos PCA personalizados salvos com sucesso!\n")
# cat("Arquivos gerados:\n")
# cat("- pca_tf_palavras_plotly.html\n")
# cat("- pca_tf_anos_plotly.html\n")
# cat("- pca_binario_palavras_plotly.html\n")
# cat("- pca_binario_anos_plotly.html\n")
# cat("- pca_tfidf_palavras_plotly.html\n")
# cat("- pca_tfidf_anos_plotly.html\n")

# ===================================
# 9. RESUMO DAS ANÁLISES
# ===================================
cat("\n=== RESUMO DAS ANÁLISES PCA ===\n")

# Variância explicada pelos 2 primeiros componentes
var_tf <- sum(summary(pca_tf)$importance[2, 1:2]) * 100
var_bin <- sum(summary(pca_bin)$importance[2, 1:2]) * 100
var_tfidf <- sum(summary(pca_tfidf)$importance[2, 1:2]) * 100

cat("Variância explicada pelos 2 primeiros componentes:\n")
cat("- TF:", round(var_tf, 2), "%\n")
cat("- Binário:", round(var_bin, 2), "%\n")
cat("- TF-IDF:", round(var_tfidf, 2), "%\n")

# Número de variáveis (palavras) em cada análise
cat("\nNúmero de variáveis (palavras):\n")
cat("- TF:", ncol(matriz_tf_t), "palavras\n")
cat("- Binário:", ncol(matriz_binaria_t), "palavras\n")
cat("- TF-IDF:", ncol(matriz_tfidf_filtrada), "palavras (após filtro)\n")