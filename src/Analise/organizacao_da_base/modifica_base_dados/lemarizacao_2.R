# Certifique-se de que o dicionário de lematização esteja carregado e formatado
# Exemplo de como carregar/preparar o dicionário novamente:
# (Se você já carregou isso no início do seu script de pré-processamento, pode pular)
 library(dplyr) # Necessário para %>%
lemma_dic <- read.delim(file = "https://raw.githubusercontent.com/michmech/lemmatization-lists/master/lemmatization-pt.txt", header = FALSE, stringsAsFactors = FALSE)
names(lemma_dic) <- c("lema_dicionario", "token") # Renomear para evitar conflito e clareza no merge


# --- PASSO 1: Garantir que 'letras_anotadas_df' está disponível ---
# (Esta é a saída do seu processamento com udpipe, antes de agrupar as lemmas por música)
# Se você já rodou o código que gera letras_anotadas_df, ele deve estar na memória.

# --- PASSO 2: Juntar as anotações do udpipe com o dicionário de lemas ---

# Faremos um left_join para manter todas as palavras do udpipe e adicionar o lema do dicionário (se houver)
# A junção será feita pela coluna 'token' (a palavra original anotada pelo udpipe e também presente no dicionário)
letras_anotadas_com_dicionario <- letras_anotadas_df %>%
  # Selecionar colunas relevantes do udpipe para não poluir o data.frame intermediário
  dplyr::select(doc_id, token, lemma, upos) %>% # 'lemma' é o lema do udpipe, 'token' é a palavra original
  dplyr::left_join(lemma_dic, by = "token") # Juntar pelo 'token' (palavra original)

# --- PASSO 3: Aplicar a lógica de lematização condicional (o "refinamento") ---

# Criar a coluna 'lemma_final' usando a lógica de cascata:
# Se o dicionário tiver um lema para o token (e não for vazio), use-o.
# Caso contrário, use o lema que veio do udpipe.
letras_anotadas_final <- letras_anotadas_com_dicionario %>%
  dplyr::mutate(
    lemma_final = ifelse(
      !is.na(lema_dicionario) & lema_dicionario != "", # Condição: Se o dicionário forneceu um lema válido
      lema_dicionario,                                  # Ação TRUE: Use o lema do dicionário
      lemma                                             # Ação FALSE: Caso contrário, use o lema do udpipe
    )
  ) %>%
  # Opcional: Remover quaisquer linhas onde o lema final ainda seja NA (ex: pontuações que não foram filtradas no udpipe ou palavras desconhecidas)
  dplyr::filter(!is.na(lemma_final) & lemma_final != "")


# --- PASSO 4: Reconstruir o Corpus/DTM com os lemas "finais" ---

# Agora, 'letras_anotadas_final' tem a coluna 'lemma_final' com os lemas combinados/refinados.
# Você pode então prosseguir para agrupar por doc_id e criar seu corpus_letras_lematizadas_final
# e, consequentemente, sua DTM final.

corpus_letras_lematizadas_final <- letras_anotadas_final %>%
  dplyr::group_by(doc_id) %>%
  dplyr::summarise(lemmas_texto = paste(lemma_final, collapse = " ")) %>%
  dplyr::ungroup() # Remover agrupamento para uso posterior se necessário

# Atualizar o corpus com as letras lematizadas finais
# É importante garantir que a ordem dos documentos seja mantida conforme a original
final_corpus_para_dtm <- VCorpus(VectorSource(corpus_letras_lematizadas_final$lemmas_texto[order(as.numeric(corpus_letras_lematizadas_final$doc_id))]))

# Criar a DTM final com os lemas refinados
dtm_lemas_refinados_tf <- DocumentTermMatrix(final_corpus_para_dtm)

# Opcional: Inspecionar a nova DTM refinada
cat("\n\n=============================================\n")
cat("   DTM COM LEMAS REFINADOS (PRIMEIRAS LINHAS/COLUNAS)\n")
cat("=============================================\n")
print(inspect(dtm_lemas_refinados_tf[1:min(5, nrow(dtm_lemas_refinados_tf)), 1:min(10, ncol(dtm_lemas_refinados_tf))]))

#=======================================================================================

# Certifique-se de que a sua dtm_lemas_refinados_tf foi criada
# Se você já a criou e ela está na sua sessão R, pode seguir.
# Caso contrário, você precisará rodar o código de pré-processamento,
# lematização (udpipe + dicionário) para gerá-la primeiro.

# --- Salvar a DTM refinada como uma matriz TF ---

# Primeiro, converta a DTM (que é um objeto esparso) para uma matriz densa.
# CUIDADO: Para DTMs muito grandes, isso pode consumir bastante memória RAM.
matrix_tf_final <- as.matrix(dtm_lemas_refinados_tf)

# Salve a matriz TF final em um arquivo .rds
saveRDS(matrix_tf_final, file = "matrix_tf_final.rds")

cat("\n=============================================\n")
cat(" Matriz TF final salva como 'matrix_tf_final.rds'\n")
cat("=============================================\n")

# Para verificar as dimensões da matriz salva
print(dim(matrix_tf_final))

# Carregar a matriz TF final do arquivo .rds
#matrix_tf_final <- readRDS("matrix_tf_final.rds")

#==========================================================================

# Certifique-se de que dtm_lemas_refinados_tf está disponível
# Se você a salvou, pode carregá-la:
# dtm_lemas_refinados_tf <- readRDS("dtm_lemas_refinados_tf.rds")

# --- 1. Calcular a frequência total de cada palavra na DTM refinada ---
# Converter a DTM para uma matriz para facilitar a soma das colunas
matrix_refinada <- as.matrix(dtm_lemas_refinados_tf)

# Calcular a soma de cada coluna (cada lema)
frequencia_total_lemas_refinados <- colSums(matrix_refinada)

# --- 2. Definir o limite de frequência ---
min_frequencia_nova <- 5 # Queremos palavras que apareçam MAIS de 5 vezes

# --- 3. Filtrar as palavras (lemas) com base na nova frequência desejada ---
palavras_selecionadas_final <- names(frequencia_total_lemas_refinados[
  frequencia_total_lemas_refinados > min_frequencia_nova
])

# --- 4. Criar a nova DTM contendo apenas as palavras selecionadas ---
# Usamos a dtm_lemas_refinados_tf original e selecionamos apenas as colunas correspondentes
dtm_mais_de_5_vezes <- dtm_lemas_refinados_tf[, colnames(dtm_lemas_refinados_tf) %in% palavras_selecionadas_final]

# --- Verificações (Opcional) ---
cat("\n\n=============================================\n")
cat("   NOVA DTM: PALAVRAS COM MAIS DE 5 OCORRÊNCIAS\n")
cat("=============================================\n")

cat("Dimensões da DTM de lemas refinados original:\n")
print(dim(dtm_lemas_refinados_tf))

cat("\nNúmero de palavras selecionadas (frequência > 5): ")
print(length(palavras_selecionadas_final))

cat("\nDimensões da nova DTM (palavras > 5 ocorrências):\n")
print(dim(dtm_mais_de_5_vezes))

# Inspecionar as primeiras linhas e colunas da nova DTM
if (nrow(dtm_mais_de_5_vezes) > 0 && ncol(dtm_mais_de_5_vezes) > 0) {
  cat("\nPrimeiras 5 linhas e 10 colunas da nova DTM (se houver):\n")
  print(as.matrix(dtm_mais_de_5_vezes[1:min(5, nrow(dtm_mais_de_5_vezes)), 1:min(10, ncol(dtm_mais_de_5_vezes))]))
} else {
  cat("\nA nova DTM está vazia ou muito pequena para exibir as primeiras linhas/colunas.\n")
}

# Opcional: Salvar esta nova DTM filtrada
saveRDS(dtm_mais_de_5_vezes, file = "dtm_mais_de_5_vezes.rds")
