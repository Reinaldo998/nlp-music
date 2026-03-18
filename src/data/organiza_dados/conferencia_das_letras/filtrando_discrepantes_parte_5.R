
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

# --- Aplicar a lógica de limpeza ao dataframe CONSOLIDADO ---
# Criando novas colunas _limpo_final para as comparações
df_consolidado_limpo <- df_consolidado_musicas_iguais_artistas_diferentes %>%
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

# --- Filtrar e Categorizar em QUATRO Novos Dataframes ---

# 1. Dataframe: Músicas Iguais, Artistas Diferentes (após consolidação e limpeza final)
df_final_musicas_iguais_artistas_diferentes <- df_consolidado_limpo %>%
  filter(
    # Garante que os valores do site NÃO são NA para comparação de igualdade/diferença
    !is.na(Nome.y_limpo_final),
    !is.na(Artista.y_limpo_final),
    
    Nome.x_limpo_final == Nome.y_limpo_final,
    Artista.x_limpo_final != Artista.y_limpo_final
  )

# 2. Dataframe: Músicas Diferentes, Artistas Diferentes (após consolidação e limpeza final)
df_final_musicas_e_artistas_diferentes <- df_consolidado_limpo %>%
  filter(
    # Garante que os valores do site NÃO são NA para comparação de igualdade/diferença
    !is.na(Nome.y_limpo_final),
    !is.na(Artista.y_limpo_final),
    
    Nome.x_limpo_final != Nome.y_limpo_final,
    Artista.x_limpo_final != Artista.y_limpo_final
  )

# 3. Dataframe: Músicas e Artistas Iguais (após consolidação e limpeza final)
df_final_musicas_e_artistas_iguais <- df_consolidado_limpo %>%
  filter(
    # Garante que os valores do site NÃO são NA para comparação de igualdade/diferença
    !is.na(Nome.y_limpo_final),
    !is.na(Artista.y_limpo_final),
    
    Nome.x_limpo_final == Nome.y_limpo_final,
    Artista.x_limpo_final == Artista.y_limpo_final
  )

# 4. NOVO DATAFRAME: Dados do Site Não Encontrados (contém os NAs)
# Este dataframe captura todas as linhas que foram excluídas dos 3 grupos acima
# por terem NAs nas colunas de Nome.y, Artista.y ou Letra.
df_final_dados_web_nao_encontrados <- df_consolidado_limpo %>%
  filter(is.na(Nome.y) | is.na(Artista.y) | is.na(Letra))

# --- Exibir os resultados ---
print("--- Dataframes de Categorização Final (4 Grupos) ---")

print("1. Dataframe: Músicas Iguais, Artistas Diferentes (Pós-Consolidação):")
View(df_final_musicas_iguais_artistas_diferentes)
dim(df_final_musicas_iguais_artistas_diferentes)

print("2. Dataframe: Músicas Diferentes, Artistas Diferentes (Pós-Consolidação):")
View(df_final_musicas_e_artistas_diferentes)
dim(df_final_musicas_e_artistas_diferentes)

print("3. Dataframe: Músicas e Artistas Iguais (Pós-Consolidação):")
View(df_final_musicas_e_artistas_iguais)
dim(df_final_musicas_e_artistas_iguais)

print("4. Dataframe: Dados do Site NÃO Encontrados (Contém NAs):")
View(df_final_dados_web_nao_encontrados)
dim(df_final_dados_web_nao_encontrados)

# --- Opcional: Salvar os novos dataframes em CSVs ---
# write.csv(df_final_musicas_iguais_artistas_diferentes, "consolidado_musicas_iguais_artistas_diferentes.csv", row.names = FALSE, fileEncoding = "UTF-8")
# write.csv(df_final_musicas_e_artistas_diferentes, "consolidado_musicas_e_artistas_diferentes.csv", row.names = FALSE, fileEncoding = "UTF-8")
# write.csv(df_final_musicas_e_artistas_iguais, "consolidado_musicas_e_artistas_iguais.csv", row.names = FALSE, fileEncoding = "UTF-8")
# write.csv(df_final_dados_web_nao_encontrados, "consolidado_dados_web_nao_encontrados.csv", row.names = FALSE, fileEncoding = "UTF-8")
