# Carregar pacotes
library(dplyr)
library(tm)
library(slam)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(plotly)
library(htmlwidgets)

set.seed(123)  # Para resultados reprodutíveis

#===================================
# PREPARAÇÃO DOS DADOS
#===================================
artistas <- data_final_pt$Artista.x
anos <- data_final_pt$Ano
nomes_musicas <- data_final_pt$Musica  # Assumindo que existe esta coluna

# ----------------------------
# Função de pré-processamento
# ----------------------------
limpar_corpus <- function(texto) {
  corpus <- VCorpus(VectorSource(texto)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("pt")) %>%
    tm_map(stripWhitespace)
  return(corpus)
}

#===================================
# CRIAÇÃO DAS 3 REPRESENTAÇÕES
#===================================

# -------------------------------
# 1. Term-Document Matrix (TF bruto)
# -------------------------------
corpus_tf <- limpar_corpus(data_final_pt$Letra)
tdm_tf <- TermDocumentMatrix(corpus_tf)
matriz_tf <- as.matrix(tdm_tf)
matriz_tf_t <- t(matriz_tf)  # Transposta: músicas nas linhas, palavras nas colunas

# Calcular número total de palavras por música (soma das linhas da matriz TF)
num_palavras_tf <- rowSums(matriz_tf_t)

# -------------------------------
# 2. Matriz Binária (presença/ausência)
# -------------------------------
matriz_binaria <- matriz_tf
matriz_binaria[matriz_binaria > 0] <- 1
matriz_binaria_t <- t(matriz_binaria)

# -------------------------------
# 3. Term-Document Matrix com peso TF-IDF
# -------------------------------
corpus_tfidf <- limpar_corpus(data_final_pt$Letra)
tdm_tfidf <- TermDocumentMatrix(corpus_tfidf,
                                control = list(weighting = weightTfIdf))
matriz_tfidf <- as.matrix(tdm_tfidf)
matriz_tfidf_t <- t(matriz_tfidf)

# Remover termos com variância zero para TF-IDF
variancias <- apply(matriz_tfidf_t, 2, var)
matriz_tfidf_filtrada <- matriz_tfidf_t[, variancias > 0]

#===================================
# APLICAÇÃO DO PCA NAS 3 REPRESENTAÇÕES
#===================================

# PCA 1: TF
pca_tf <- prcomp(matriz_tf_t, scale. = TRUE)

# PCA 2: Matriz Binária
pca_bin <- prcomp(matriz_binaria_t, scale. = TRUE)

# PCA 3: TF-IDF
pca_tfidf <- prcomp(matriz_tfidf_filtrada, scale. = TRUE)

#===================================
# FUNÇÃO PARA CRIAR GRÁFICOS PLOTLY
#===================================
criar_grafico_pca_plotly <- function(pca_resultado, titulo, num_palavras = NULL, nomes_musicas = NULL, artistas = NULL, anos = NULL) {
  # Extrair coordenadas das duas primeiras componentes
  n_obs <- nrow(pca_resultado$x)
  
  # Criar vetores com o tamanho correto se não fornecidos ou se tamanho diferente
  if (is.null(num_palavras) || length(num_palavras) != n_obs) {
    num_palavras <- rep("N/A", n_obs)
  }
  if (is.null(nomes_musicas) || length(nomes_musicas) != n_obs) {
    nomes_musicas <- paste("Música", 1:n_obs)
  }
  if (is.null(artistas) || length(artistas) != n_obs) {
    artistas <- rep("N/A", n_obs)
  }
  if (is.null(anos) || length(anos) != n_obs) {
    anos <- rep("N/A", n_obs)
  }
  
  # Garantir que todos tenham o mesmo tamanho
  num_palavras <- num_palavras[1:n_obs]
  nomes_musicas <- nomes_musicas[1:n_obs]
  artistas <- artistas[1:n_obs]
  anos <- anos[1:n_obs]
  
  coordenadas <- data.frame(
    PC1 = pca_resultado$x[,1],
    PC2 = pca_resultado$x[,2],
    NumPalavras = num_palavras,
    Musica = nomes_musicas,
    Artista = artistas,
    Ano = anos
  )
  
  # Calcular a qualidade de representação (cos²)
  cos2 <- (coordenadas$PC1^2 + coordenadas$PC2^2) / 
    (coordenadas$PC1^2 + coordenadas$PC2^2 + apply(pca_resultado$x[,-(1:2)]^2, 1, sum))
  coordenadas$cos2 <- cos2
  
  # Calcular variância explicada
  var_explicada <- summary(pca_resultado)$importance[2, 1:2] * 100
  
  # Criar hover text
  hover_text <- ~paste(
    "Música:", Musica,
    "<br>Artista:", ifelse(is.null(Artista) | is.na(Artista) | Artista == "N/A", "N/A", Artista),
    "<br>Total de palavras:", ifelse(is.null(NumPalavras) | is.na(NumPalavras) | NumPalavras == "N/A", "N/A", NumPalavras),
    "<br>Ano:", ifelse(is.null(Ano) | is.na(Ano) | Ano == "N/A", "N/A", Ano),
    "<br>Qualidade (cos²):", round(cos2, 3)
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
      color = ~cos2,
      colorscale = list(
        c(0, "#00AFBB"),    # Azul
        c(0.5, "#E7B800"),  # Amarelo
        c(1, "#FC4E07")     # Vermelho
      ),
      opacity = 0.8,
      line = list(width = 1, color = "white"),
      colorbar = list(
        title = "Qualidade<br>(cos²)",
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

#===================================
# CRIAÇÃO DOS GRÁFICOS PLOTLY
#===================================

# Gráfico PCA TF
grafico_pca_tf <- criar_grafico_pca_plotly(
  pca_resultado = pca_tf,
  titulo = "PCA - Representação TF (Term Frequency)",
  num_palavras = num_palavras_tf,
  nomes_musicas = nomes_musicas,
  artistas = artistas,
  anos = anos
)

# Gráfico PCA Binário
grafico_pca_bin <- criar_grafico_pca_plotly(
  pca_resultado = pca_bin,
  titulo = "PCA - Representação Binária (Presença/Ausência)",
  num_palavras = num_palavras_tf,  # Usando o mesmo número de palavras como referência
  nomes_musicas = nomes_musicas,
  artistas = artistas,
  anos = anos
)


# Gráfico PCA TF-IDF - CORRIGIDO
grafico_pca_tfidf <- criar_grafico_pca_plotly(
  pca_resultado = pca_tfidf,
  titulo = "PCA - Representação TF-IDF",
  num_palavras = num_palavras_tf,  # Não filtra aqui!
  nomes_musicas = nomes_musicas,    # Não filtra aqui!
  artistas = artistas,              # Não filtra aqui!
  anos = anos                       # Não filtra aqui!
)
#===================================
# EXIBIÇÃO DOS GRÁFICOS
#===================================
print("=== GRÁFICO PCA TF ===")
print(grafico_pca_tf)

print("=== GRÁFICO PCA BINÁRIO ===")
print(grafico_pca_bin)

print("=== GRÁFICO PCA TF-IDF ===")
print(grafico_pca_tfidf)

#===================================
# GRÁFICOS DE VARIÂNCIA EXPLICADA
#===================================
# Usando factoextra para mostrar a variância explicada
print("=== VARIÂNCIA EXPLICADA - TF ===")
print(fviz_eig(pca_tf, main = "PCA TF - Variância Explicada"))

print("=== VARIÂNCIA EXPLICADA - BINÁRIO ===")
print(fviz_eig(pca_bin, main = "PCA Binário - Variância Explicada"))

print("=== VARIÂNCIA EXPLICADA - TF-IDF ===")
print(fviz_eig(pca_tfidf, main = "PCA TF-IDF - Variância Explicada"))

#===================================
# SALVAMENTO DOS GRÁFICOS (OPCIONAL)
#===================================
# Salvar gráficos como arquivos HTML
#saveWidget(
#  grafico_pca_tf, 
#  file = "pca_tf_plotly.html",
#  selfcontained = TRUE
#)
#
#saveWidget(
#  grafico_pca_bin, 
#  file = "pca_binario_plotly.html",
#  selfcontained = TRUE
#)
#
#saveWidget(
#  grafico_pca_tfidf, 
#  file = "pca_tfidf_plotly.html",
#  selfcontained = TRUE
#)
#
#cat("Gráficos PCA salvos com sucesso!\n")
#cat("Arquivos gerados:\n")
#cat("- pca_tf_plotly.html\n")
#cat("- pca_binario_plotly.html\n")
#cat("- pca_tfidf_plotly.html\n")
#
##===================================
# RESUMO DAS ANÁLISES
#===================================
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