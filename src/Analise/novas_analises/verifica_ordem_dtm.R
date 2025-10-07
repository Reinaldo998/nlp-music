# Certifique-se de que os dataframes e o corpus estão disponíveis na sua sessão.
library(dplyr)
library(tm)

# --- VERIFICAÇÃO DE ALINHAMENTO POR CONTEÚDO (Prova Definitiva) ---
cat("\n--- Verificação de Conteúdo: Dataframe de Lemmas vs. Corpus ---\n")

# Escolha a linha para verificar (ex: a primeira linha, por ser consistente)
linha_para_verificar <- 2

# 1. Obtenha o Codigo e o texto de lemmas a partir do dataframe
codigo_do_df <- letras_lematizadas_por_musica$doc_id[linha_para_verificar]
texto_do_df <- letras_lematizadas_por_musica$lemmas_texto[linha_para_verificar]

# 2. Obtenha o Codigo e o texto de lemmas a partir do Corpus
codigo_do_corpus <- meta(corpus_letras_lematizadas[[linha_para_verificar]], "id")
texto_do_corpus <- as.character(corpus_letras_lematizadas[[linha_para_verificar]])

# 3. Compare os Códigos e o Conteúdo dos textos
#as.character(codigo_do_df) == codigo_do_corpus &&
if ( texto_do_df == texto_do_corpus) {
  cat("OK. O conteúdo da linha", linha_para_verificar, "está perfeitamente alinhado entre o dataframe de lemmas e o Corpus.\n")
} else {
  cat("ERRO. O conteúdo da linha", linha_para_verificar, "NÃO está alinhado.\n")
  cat("ID do DataFrame:", codigo_do_df, "\n")
  cat("ID do Corpus:", codigo_do_corpus, "\n")
  cat("Texto do DataFrame:", strtrim(texto_do_df, 50), "...\n")
  cat("Texto do Corpus:", strtrim(texto_do_corpus, 50), "...\n")
  stop("Falha na verificação de alinhamento por conteúdo.")
}




# Os Códigos (doc_id) em letras_lematizadas_por_musica, que é a fonte dos nomes
codigos_df <- as.character(letras_lematizadas_por_musica$doc_id)

# Os nomes das linhas da matriz_dtm
codigos_matriz <- row.names(matrix_dtm)

cat("\n--- Verificação de Alinhamento: Dataframe de Lemmas vs. Matrix DTM ---\n")

# Compara se os vetores de códigos são idênticos em ordem e valor
if (isTRUE(all.equal(codigos_df, codigos_matriz))) {
  cat("OK. Os nomes das linhas da matriz estão perfeitamente alinhados com os Códigos do dataframe de lemmas.\n")
} else {
  cat("ERRO: Os nomes das linhas da matriz não correspondem.\n")
  
  # Para depuração:
  print("Primeiros 10 Códigos do DF de Lemmas:")
  print(head(codigos_df, 10))
  print("Primeiros 10 Nomes de Linha da Matriz:")
  print(head(codigos_matriz, 10))
  
  stop("Falha na verificação de alinhamento DTM.")
}