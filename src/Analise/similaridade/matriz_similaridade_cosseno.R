# Certifique-se de que o pacote 'lsa' está carregado
library(lsa)

# Assumimos que a matriz 'dtm_tf_normalizada' já foi criada
# nas etapas anteriores do seu código.

# --- Calcular a Matriz de Similaridade de Cosseno para TODAS as Músicas ---
# A similaridade de cosseno é calculada entre os vetores de cada documento.
# A função 'cosine' espera que os documentos estejam nas colunas.
# Por isso, transpomos a matriz usando t().
matriz_sim_cosseno_total <- cosine(t(dtm_tf_normalizada))

row.names(matriz_sim_cosseno_total)
colnames(matriz_sim_cosseno_total)
row.names(dtm_tf_normalizada)
row.names(dtm_matrix)

# --- Inspecionar a Matriz de Similaridade ---
print("Matriz de similaridade de cosseno para todas as músicas criada com sucesso.")
print("Dimensões da matriz de similaridade (N x N, onde N é o número de músicas):")
print(dim(matriz_sim_cosseno_total))
dim(dtm_tf_normalizada)
# Exibir uma pequena amostra da matriz de similaridade (opcional)
if (nrow(matriz_sim_cosseno_total) > 0 && ncol(matriz_sim_cosseno_total) > 0) {
  print("\nAmostra da matriz de similaridade (as primeiras 5x5 células):")
  print(matriz_sim_cosseno_total[1:min(5, nrow(matriz_sim_cosseno_total)), 1:min(5, ncol(matriz_sim_cosseno_total))])
}

# Opcional: Salvar a matriz de similaridade para uso futuro
#write.csv(matriz_sim_cosseno_total, "matriz_sim_cosseno_total.csv", row.names = TRUE)
# OU, se for muito grande, salvar no formato nativo do R:
saveRDS(matriz_sim_cosseno_total, file = "matriz_sim_cosseno_total.rds")

# A matriz original é preservada, criamos uma cópia para modificação
matriz_sim_inferior <- matriz_sim_cosseno_total

# Usar lower.tri para obter os índices da triangular inferior,
# incluindo ou não a diagonal. No seu caso, queremos sem a diagonal.
# upper.tri() também poderia ser usado.
# Para manter apenas a triangular inferior e zerar o resto, fazemos:
matriz_sim_inferior[upper.tri(matriz_sim_inferior, diag = TRUE)] <- NA

# Outra forma seria criar um dataframe 'long' com os pares
# df_sim_pares <- as.data.frame(matriz_sim_cosseno_total)
# df_sim_pares$doc_id1 <- row.names(df_sim_pares)
# df_sim_pares %>%
#   pivot_longer(cols = -doc_id1, names_to = "doc_id2", values_to = "similaridade") %>%
#   filter(doc_id1 < doc_id2) # Pega apenas os pares únicos

# --- Inspecionar a nova matriz ---
print("Nova matriz de similaridade apenas com a triangular inferior:")
View(matriz_sim_inferior)

# Para inspecionar uma amostra (as primeiras 5x5 células)
if (nrow(matriz_sim_inferior) > 0 && ncol(matriz_sim_inferior) > 0) {
  print("\nAmostra da matriz:")
  print(matriz_sim_inferior[1:min(5, nrow(matriz_sim_inferior)), 1:min(5, ncol(matriz_sim_inferior))])
}

# Opcional: Salvar a nova matriz para uso futuro
#saveRDS(matriz_sim_inferior, file = "matriz_sim_cosseno_inferior.rds")

# Note que os Códigos precisam ser do tipo 'character' para indexar a matriz.
musica_1_codigo <- "6666"
musica_2_codigo <- "6701"

# Acesse a matriz de similaridade usando os Códigos das músicas.
# A sintaxe é matriz[linha, coluna]
similaridade <- matriz_sim_cosseno_total[musica_1_codigo, musica_2_codigo]

# Exibir o resultado
print(paste0(
  "A similaridade de cosseno entre a música ", musica_1_codigo,
  " e a música ", musica_2_codigo, " é: ", similaridade
))

"6701" %in% row.names(dtm_tf_normalizada)
"6394" %in% row.names(dtm_filtrada)
