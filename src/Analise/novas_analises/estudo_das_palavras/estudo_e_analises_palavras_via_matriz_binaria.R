library(dplyr)
library(tm)

# --- Supondo que 'frequencia_total_palavras' e 'dtm_letras_tf' estão disponíveis. ---

# --- 1. Definir os Limites do Filtro LDA ---
min_freq_lda <- 10    #
max_freq_lda <- 440   #


# --- 2. Identificar os Termos a Serem MANTIDOS ---
# Seleciona os nomes (termos) cuja frequência total está dentro do intervalo.
palavras_para_lda <- names(frequencia_documentos[
  frequencia_documentos >= min_freq_lda &
    frequencia_documentos <= max_freq_lda
])


# --- 3. Criar a DTM Final para LDA (Subsetting) ---
# Filtra a DTM original (dtm_letras_tf) para incluir APENAS as colunas selecionadas.
dtm_final_lda <- dtm_letras_tf[, colnames(dtm_letras_tf) %in% palavras_para_lda]

dtm_final_lda <- dtm_final_lda[rowSums(as.matrix(dtm_final_lda)) > 0, ]


# --- 4. Verificação Final ---
cat("\n--- RESULTADO DA FILTRAGEM PARA LDA ---\n")
cat(paste0("Filtro Aplicado: [", min_freq_lda, " a ", max_freq_lda, "]\n"))
cat(paste0("Dimensões da DTM Final (Músicas x Termos): ", paste(dim(dtm_final_lda), collapse = "x"), "\n"))
cat(paste0("Total de termos (palavras) no vocabulário final: ", ncol(dtm_final_lda), "\n"))

#saveRDS(dtm_final_lda, file = "dtm_final_unicas.rds")

# Este é o objeto pronto para ser usado na função LDA():
# ap_lda_atual <- LDA(dtm_final_lda, k = 10, method = "Gibbs")

# 1. Salva a configuração atual de print para restaurar depois
original_max_print <- getOption("max.print")

# 2. Obtém o número total de palavras
total_termos <- ncol(dtm_final_lda)

# 3. Temporariamente define o limite de impressão para infinito
options(max.print = 1000000)

# --- Exibir a lista completa de palavras (Termos) ---
cat(paste0("VOCABULÁRIO FINAL NA DTM (Total: ", total_termos, " termos)\n"))

# Usa a função colnames() para extrair todas as palavras
palavras_na_dtm <- colnames(dtm_final_lda)
palavras_na_dtm <- colnames(matrix_dtm)
palavras_na_dtm <- colnames(dtm_filtrada_stem)
print(palavras_na_dtm)
length(palavras_na_dtm)
# 1. Obter os vocabulários (nomes das colunas) das duas DTMs
vocabulario_lda <- colnames(dtm_final_lda)
vocabulario_filtrada <- colnames(dtm_filtrada)


palavras_unicas_lda <- setdiff(vocabulario_lda, vocabulario_filtrada)
palavras_unicas_filtrada <- setdiff(vocabulario_filtrada, vocabulario_lda)

length(palavras_unicas_lda)
length(palavras_unicas_filtrada)


print(palavras_unicas_lda)
print(palavras_unicas_filtrada)

# 4. Restaura o limite de impressão
options(max.print = original_max_print)

palavras_a_remover <- c("refr","ooh","prar","pro","prir","refrao")

length(palavras_a_remover)
length(palavras_na_dtm)
# --- 2. Remover todas as colunas presentes no vetor ---
# A lógica '!(colnames(dtm_final_lda) %in% palavras_a_remover)' cria um filtro
# que é 'TRUE' apenas para as colunas que DEVEM ser mantidas.
dtm_final_lda <- dtm_final_lda[, !(colnames(dtm_final_lda) %in% palavras_a_remover)]

# --- 3. Verificação Final ---
cat(paste0("Nova dimensão da DTM: ", paste(dim(dtm_final_lda), collapse = "x"), "\n"))
