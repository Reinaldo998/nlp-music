# 1. Calcule a Frequência de Documentos (DF)
# DF = Número de músicas em que a palavra aparece (matriz binária)
dtm_binaria <- (as.matrix(dtm_letras_tf) > 0) * 1
frequencia_documentos <- colSums(dtm_binaria)

# 2. Calcule a Frequência de Termos (TF)
# TF = Contagem total de vezes que a palavra aparece
frequencia_termos <- colSums(as.matrix(dtm_letras_tf))

total_palavras_unicas <- length(frequencia_documentos)
# --- 2. Definir os limites do filtro (Ajuste estes valores) ---


# Certifique-se de que o vetor 'frequencia_total_palavras' está na sua sessão.

# --- FUNÇÃO PRINCIPAL: ANÁLISE DE IMPACTO (COM MAX E MIN) ---

analisar_impacto_filtro <- function(frequencias, min_freq, max_freq) {
  
  # 1. Obter estatísticas globais e descritivas
  total_palavras_unicas <- length(frequencias)
  
  # Calcular Frequências Máxima e Mínima (desprezando zeros, embora não deva haver)
  max_freq_total <- max(frequencias, na.rm = TRUE)
  min_freq_total <- min(frequencias[frequencias > 0], na.rm = TRUE)
  
  # Calcular Média e Mediana
  media_freq <- mean(frequencias, na.rm = TRUE)
  mediana_freq <- median(frequencias, na.rm = TRUE)
  
  # 2. Classificar e contar usando lógica vetorial
  num_excluidos_raros <- sum(frequencias < min_freq)
  num_excluidos_comuns <- sum(frequencias > max_freq)
  num_mantidos <- sum(frequencias >= min_freq & frequencias <= max_freq)
  
  # 3. Calcular porcentagens
  pct_excluidos_raros <- (num_excluidos_raros / total_palavras_unicas) * 100
  pct_excluidos_comuns <- (num_excluidos_comuns / total_palavras_unicas) * 100
  pct_mantidos <- (num_mantidos / total_palavras_unicas) * 100
  
  # 4. Imprimir o resumo final
  
  # --- ESTATÍSTICAS DESCRITIVAS ---
  cat("\n--- ESTATÍSTICAS DESCRITIVAS DO VOCABULÁRIO ---\n")
  cat(paste0("Total de Palavras Únicas: ", total_palavras_unicas, "\n"))
  cat(paste0("Frequência MÁXIMA (Palavra mais comum): ", max_freq_total, "\n"))
  cat(paste0("Frequência MÍNIMA (Palavra mais rara, > 0): ", min_freq_total, "\n"))
  cat(paste0("Frequência MÉDIA: ", round(media_freq, 2), "\n"))
  cat(paste0("Frequência MEDIANA: ", round(mediana_freq, 2), "\n"))
  cat("-----------------------------------------------------------------\n")
  
  # --- ANÁLISE DE IMPACTO DO FILTRO ---
  cat(paste0("ANÁLISE DE IMPACTO DO FILTRO: [", min_freq, " a ", max_freq, "]\n"))
  
  cat("\n1. MANTIDAS (FREQUÊNCIA >= ", min_freq, " e <= ", max_freq, "):\n")
  cat(paste0("   - Contagem: ", num_mantidos, "\n"))
  cat(paste0("   - Porcentagem: ", round(pct_mantidos, 2), "%\n"))
  
  cat("\n2. EXCLUÍDAS (MUITO RARAS: < ", min_freq, "):\n")
  cat(paste0("   - Contagem: ", num_excluidos_raros, "\n"))
  cat(paste0("   - Porcentagem: ", round(pct_excluidos_raros, 2), "%\n"))
  
  cat("\n3. EXCLUÍDAS (MUITO COMUNS: > ", max_freq, "):\n")
  cat(paste0("   - Contagem: ", num_excluidos_comuns, "\n"))
  cat(paste0("   - Porcentagem: ", round(pct_excluidos_comuns, 2), "%\n"))
  cat("=================================================================\n")
}



# --- EXEMPLO DE USO ---
analisar_impacto_filtro(frequencia_documentos, min_freq = 10, max_freq = 440)

analisar_impacto_filtro(frequencia_termos , min_freq = 30, max_freq = 1000)
boxplot(frequencia_documentos)
#hist(sort(frequencia_documentos)[1:13000])

hist(sort(frequencia_termos)[1:13000])


# Certifique-se de que a sua DTM de FREQUÊNCIA (dtm_letras_tf ou dtm_filtrada) está na sessão.

print(paste0("A frequência máxima de uma palavra em uma ÚNICA MÚSICA (na DTM TF) é: ", max(as.matrix(dtm_matrix_binary))))



# 3. Ordene e compare os top 10 valores
df_ordenado <- sort(frequencia_documentos, decreasing = TRUE)
tf_ordenado <- sort(frequencia_termos, decreasing = TRUE)

# Crie um dataframe para inspeção lado a lado
df_comparacao <- data.frame(
  Posicao = 1:50,
  DF_Frequencia = df_ordenado[1:50],
  TF_Frequencia = tf_ordenado[1:50]
)

print("\n--- COMPARAÇÃO DOS TOP 20 VALORES DE FREQUÊNCIA ---")
print("Seus valores NÃO são idênticos. O DF é o número de músicas e o TF é a contagem total.")
View(df_comparacao)
print(df_comparacao)




