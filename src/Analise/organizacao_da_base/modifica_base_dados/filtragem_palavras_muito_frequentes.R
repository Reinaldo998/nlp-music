# ===================================
# 1. FILTRAR MATRIZ POR PALAVRAS MUITO FREQUENTES
# ===================================
matriz_tf_t_1<-matriz_tf_t
matriz_tf_t<-matrix_dtm
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
top_15_palavras <- sort(palavras_muito_frequentes, decreasing = TRUE)[1:35]
cat("=== TOP 15 PALAVRAS MAIS FREQUENTES ===\n")
print(top_15_palavras)


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