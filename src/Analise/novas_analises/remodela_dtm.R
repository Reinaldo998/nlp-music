# --- Carregar pacotes necessários (se ainda não estiverem) ---
library(dplyr) # Para a função filter()

# --- Suponha que 'data_final_pt' e 'dtm_filtrada' já estão carregadas na memória ---
# 'data_final_pt' deve ter uma coluna chamada "ID" que contém os identificadores únicos das músicas.
# 'dtm_filtrada' (sua matriz DTM) deve ter os nomes das linhas (rownames) que correspondem a esses IDs.

# =================================================================================
# 1. Defina os IDs das músicas que você deseja REMOVER
#    Estes IDs serão usados para criar novas versões dos seus dataframes/matriz.
# =================================================================================
ids_a_remover <- c(5988, 9923, 6576, 6769, 9633, 9595, 1495, 6073, 7290, 9997, 1425, 9321, 6128, 6319, 340, 9285, 9095, 7689, 9423, 9675, 6589, 7776, 144, 6881, 6375, 9208, 9098, 6998, 76100)
length(ids_a_remover)
# =================================================================================
# 2. Criar um NOVO DataFrame 'data_final_pt_2' sem as linhas dos IDs a remover
# =================================================================================
# Garante que a coluna "ID" em data_final_pt é do tipo CHARACTER para uma comparação segura.
# Cria uma cópia temporária para não modificar o original antes de filtrar.
data_final_pt_temp <- data_final_pt
data_final_pt_temp$ID <- as.character(data_final_pt_temp$ID)

# Filtra o dataframe, mantendo apenas as linhas onde o ID NÃO está na lista de IDs a remover.
data_final_pt_2 <- data_final_pt_temp %>%
  filter(!ID %in% as.character(ids_a_remover)) # Converte ids_a_remover para caractere também

cat("DataFrame 'data_final_pt_2' criado. Número de linhas restantes:", nrow(data_final_pt_2), "\n")

# =================================================================================
# 3. Criar uma NOVA Matriz DTM 'dtm_filtrada_2' sem as linhas dos IDs a remover
# =================================================================================
# Cria uma cópia temporária para não modificar o original antes de filtrar.
dtm_filtrada_temp <- dtm_filtrada
# Garante que os nomes das linhas de dtm_filtrada são do tipo CHARACTER para comparação.
rownames(dtm_filtrada_temp) <- as.character(rownames(dtm_filtrada_temp))

# Filtra a matriz, mantendo apenas as linhas cujos nomes NÃO estão na lista de IDs a remover.
dtm_filtrada_2 <- dtm_filtrada_temp[!(rownames(dtm_filtrada_temp) %in% as.character(ids_a_remover)), ]

cat("Matriz 'dtm_filtrada_2' criada. Número de linhas restantes:", nrow(dtm_filtrada_2), "\n")


#saveRDS(dtm_filtrada, file = "dtm_filtrada_tf_1_set.rds")
# =================================================================================
# 4. Resultados
# =================================================================================
# As variáveis resultantes são:
# - 'data_final_pt_2': Seu dataframe original sem as músicas removidas.
# - 'dtm_filtrada_2': Sua matriz DTM sem as linhas das músicas removidas.

# Opcional: Visualizar as primeiras linhas dos data frames filtrados
# head(data_final_pt_2)
# head(dtm_filtrada_2)

#saveRDS(data_final_pt, file = "data_final_pt_1_set.rds")
