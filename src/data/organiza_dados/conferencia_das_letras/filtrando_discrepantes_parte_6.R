library(dplyr)

# Função auxiliar para remover acentos
remove_acentos <- function(texto) {
  if (is.character(texto)) {
    chartr(
      "áéíóúÁÉÍÓÚàèìòùÀÈÌÒÙãõñÃÕÑâêîôûÂÊÎÔÛçÇ",
      "aeiouAEIOUaeiouAEIOUaonAONaeiouAEIOUcC",
      texto
    )
  } else {
    texto # Retorna o valor original se não for caractere
  }
}

# --- Aplicar a lógica de limpeza ao dataframe CONSOLIDADO (Músicas e Artistas Diferentes) ---
# ATENÇÃO: O dataframe de entrada agora é df_consolidado_musicas_e_artistas_diferentes
df_consolidado_limpo_from_ma_diff <- df_consolidado_musicas_e_artistas_diferentes %>%
  mutate(
    # Limpeza para Nome.x (Original)
    Nome.x_limpo_final = tolower(trimws(Nome.x)),
    Nome.x_limpo_final = gsub("\\s*\\([^\\)]+\\)", "", Nome.x_limpo_final), # Remover parênteses
    Nome.x_limpo_final = gsub("[[:punct:]]", "", Nome.x_limpo_final),     # Remover pontuação
    Nome.x_limpo_final = sapply(Nome.x_limpo_final, remove_acentos),      # Remover acentuação
    
    # Limpeza para Artista.x (Original)
    Artista.x_limpo_final = tolower(trimws(Artista.x)),
    Artista.x_limpo_final = gsub("\\s*\\([^\\)]+\\)", "", Artista.x_limpo_final),
    Artista.x_limpo_final = gsub("[[:punct:]]", "", Artista.x_limpo_final),
    Artista.x_limpo_final = sapply(Artista.x_limpo_final, remove_acentos),
    
    # Limpeza para Nome.y (Extraído do Site) - Lidar com NAs
    # As funções tolower, trimws, gsub, sapply propagam NAs, o que é o comportamento desejado.
    Nome.y_limpo_final = tolower(trimws(Nome.y)),
    Nome.y_limpo_final = gsub("\\s*\\([^\\)]+\\)", "", Nome.y_limpo_final),
    Nome.y_limpo_final = gsub("[[:punct:]]", "", Nome.y_limpo_final),
    Nome.y_limpo_final = sapply(Nome.y_limpo_final, remove_acentos),
    
    # Limpeza para Artista.y (Extraído do Site) - Lidar com NAs
    Artista.y_limpo_final = tolower(trimws(Artista.y)),
    Artista.y_limpo_final = gsub("\\s*\\([^\\)]+\\)", "", Artista.y_limpo_final),
    Artista.y_limpo_final = gsub("[[:punct:]]", "", Artista.y_limpo_final),
    Artista.y_limpo_final = sapply(Artista.y_limpo_final, remove_acentos)
  )

# --- Filtrar e Categorizar em QUATRO Novos Dataframes (para o caso M/A Diferentes) ---

# 1. Dataframe: Músicas Iguais, Artistas Diferentes (após consolidação e limpeza final)
df_final_musicas_iguais_artistas_diferentes_from_ma_diff <- df_consolidado_limpo_from_ma_diff %>%
  filter(
    # Garante que os valores do site NÃO são NA para comparação de igualdade/diferença
    !is.na(Nome.y_limpo_final),
    !is.na(Artista.y_limpo_final),
    
    Nome.x_limpo_final == Nome.y_limpo_final,
    Artista.x_limpo_final != Artista.y_limpo_final
  )

# 2. Dataframe: Músicas Diferentes, Artistas Diferentes (após consolidação e limpeza final)
df_final_musicas_e_artistas_diferentes_from_ma_diff <- df_consolidado_limpo_from_ma_diff %>%
  filter(
    # Garante que os valores do site NÃO são NA para comparação de igualdade/diferença
    !is.na(Nome.y_limpo_final),
    !is.na(Artista.y_limpo_final),
    
    Nome.x_limpo_final != Nome.y_limpo_final,
    Artista.x_limpo_final != Artista.y_limpo_final
  )

# 3. Dataframe: Músicas e Artistas Iguais (após consolidação e limpeza final)
df_final_musicas_e_artistas_iguais_from_ma_diff <- df_consolidado_limpo_from_ma_diff %>%
  filter(
    # Garante que os valores do site NÃO são NA para comparação de igualdade/diferença
    !is.na(Nome.y_limpo_final),
    !is.na(Artista.y_limpo_final),
    
    Nome.x_limpo_final == Nome.y_limpo_final,
    Artista.x_limpo_final == Artista.y_limpo_final
  )

# 4. NOVO DATAFRAME: Dados do Site Não Encontrados (contém os NAs)
df_final_dados_web_nao_encontrados_from_ma_diff <- df_consolidado_limpo_from_ma_diff %>%
  filter(is.na(Nome.y) | is.na(Artista.y) | is.na(Letra))

# --- Exibir os resultados (para o caso Músicas e Artistas Diferentes) ---
print("--- Dataframes de Categorização Final (4 Grupos) para Músicas e Artistas Diferentes ---")

print("1. Dataframe (M/A Diferentes): Músicas Iguais, Artistas Diferentes (Pós-Consolidação):")
View(df_final_musicas_iguais_artistas_diferentes_from_ma_diff)
dim(df_final_musicas_iguais_artistas_diferentes_from_ma_diff)

print("2. Dataframe (M/A Diferentes): Músicas Diferentes, Artistas Diferentes (Pós-Consolidação):")
View(df_final_musicas_e_artistas_diferentes_from_ma_diff)
dim(df_final_musicas_e_artistas_diferentes_from_ma_diff)

print("3. Dataframe (M/A Diferentes): Músicas e Artistas Iguais (Pós-Consolidação):")
View(df_final_musicas_e_artistas_iguais_from_ma_diff)
dim(df_final_musicas_e_artistas_iguais_from_ma_diff)

print("4. Dataframe (M/A Diferentes): Dados do Site NÃO Encontrados (Contém NAs):")
View(df_final_dados_web_nao_encontrados_from_ma_diff)
dim(df_final_dados_web_nao_encontrados_from_ma_diff)

# --- Opcional: Salvar os novos dataframes em CSVs (para o caso Músicas e Artistas Diferentes) ---
# write.csv(df_final_musicas_iguais_artistas_diferentes_from_ma_diff, "consolidado_miad_from_mad.csv", row.names = FALSE, fileEncoding = "UTF-8")
# write.csv(df_final_musicas_e_artistas_diferentes_from_ma_diff, "consolidado_mad_from_mad.csv", row.names = FALSE, fileEncoding = "UTF-8")
# write.csv(df_final_musicas_e_artistas_iguais_from_ma_diff, "consolidado_maig_from_mad.csv", row.names = FALSE, fileEncoding = "UTF-8")
# write.csv(df_final_dados_web_nao_encontrados_from_ma_diff, "consolidado_na_from_mad.csv", row.names = FALSE, fileEncoding = "UTF-8")