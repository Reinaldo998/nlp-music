# Certifique-se de que a matriz 'S' está disponível na sua sessão.

# A última linha da matriz 'S' contém as atribuições de grupo da última iteração.
ultima_iteracao_Sj <- S[nrow(S), ]
table(ultima_iteracao_Sj)
# Você pode então usar 'ultima_iteracao_Sj' em vez de 'clust_f'
# para criar o seu dataframe df_id_grupo.
df_id_grupo_ultima_iteracao <- data.frame(
  ID_musica = as.character(row.names(dtm_filtrada)),
  Grupo_final = ultima_iteracao_Sj
)

# ... (o restante do seu código de análise) ...
#data_dp_2<-data_dp
data_dp <- data_final_pt %>%
  left_join(df_id_grupo_ultima_iteracao, by = c("ID" = "ID_musica"))
