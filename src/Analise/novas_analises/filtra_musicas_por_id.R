codigos_para_visualizar <- df_musicas_iguais_artistas_diferentes_final$Codigo#c(6154,6173,6222,6319,6466,6476,6719,7192,7690,8230,9965,529,6226)
# --- 2. Filtre o dataframe para exibir apenas as músicas com esses Códigos ---
# O operador %in% verifica se o valor de 'Codigo' está presente no vetor 'codigos_para_visualizar'.
#df_musicas_selecionadas_view <- df_final_total_combinado_simples %>%
#  filter(Codigo %in% codigos_para_visualizar)

# --- 3. Visualize o resultado ---
print("Visualização de músicas selecionadas por Código:")
View(df_final_total_combinado_simples %>%
       filter(Codigo %in% codigos_para_visualizar))

dim(df_final_total_combinado_simples %>%
       filter(Codigo %in% codigos_para_visualizar))

