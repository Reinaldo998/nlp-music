# Certifique-se de que o pacote 'Matrix' está carregado
library(Matrix)

# No R, após ter dtm_filtrada (ou dtm_lemas_refinados_tf) pronta:

# --- 1. Salvar a DTM filtrada como uma matriz esparsa no formato Matrix Market (.mtx) ---
dtm_to_export <- dtm_filtrada # Use a DTM que você quer exportar

# Certifique-se de que não há colunas/linhas completamente zero na DTM.
# Isso é importante para evitar problemas em outras ferramentas.
dtm_to_export <- dtm_to_export[rowSums(as.matrix(dtm_to_export)) > 0, ]
dtm_to_export <- dtm_to_export[, colSums(as.matrix(dtm_to_export)) > 0]

# Salvar o vocabulário (nomes das colunas da DTM)
write.csv(data.frame(term = colnames(dtm_to_export)), "vocab_dtm_filtrada.csv", row.names = FALSE)

# Convertendo a DocumentTermMatrix para um formato que writeMM entende:
# Primeiro para uma matriz densa e depois para uma matriz esparsa padrão do pacote Matrix.
# CUIDADO: O passo 'as.matrix()' pode consumir muita RAM para DTMs gigantes.
dtm_for_export_format <- as(as.matrix(dtm_to_export), "CsparseMatrix")

# Salvar a DTM no formato Matrix Market usando writeMM
writeMM(dtm_for_export_format, "dtm_filtrada.mtx")

cat("DTM salva como 'dtm_filtrada.mtx' e vocabulário como 'vocab_dtm_filtrada.csv'.\n")

# --- 2. Salvar o data.frame original com as informações das músicas como CSV ---
write.csv(data_final_pt, "data_final_pt_original.csv", row.names = FALSE)
cat("DataFrame original salvo como 'data_final_pt_original.csv'.\n")