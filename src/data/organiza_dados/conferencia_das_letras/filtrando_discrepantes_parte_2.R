library(dplyr) # Certifique-se que dplyr está carregado

# Certifique-se de que df_final_total_combinado_simples está carregado

# --- Função auxiliar para remover acentos ---
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

# --- 1. Limpeza dos nomes das músicas e dos artistas em df_final_total_combinado_simples ---
# Converte para minúsculas, remove espaços em branco extras, parênteses, pontuação e acentuação
df_final_total_combinado_simples_limpo <- df_final_total_combinado_simples %>%
  mutate(
    # Limpeza para Nome_Original
    Nome_Original_limpo = tolower(trimws(Nome_Original)),
    Artista_Original_limpo = tolower(trimws(Artista_Original)),
    # Limpeza para Nome_Site
    Nome_Site_limpo = tolower(trimws(Nome_Site)),
    Artista_Site_limpo = tolower(trimws(Artista_Site))
  ) %>%
  # Remover texto entre parênteses e os próprios parênteses
  mutate(
    Nome_Original_limpo = gsub("\\s*\\([^\\)]+\\)", "", Nome_Original_limpo),
    Nome_Site_limpo = gsub("\\s*\\([^\\)]+\\)", "", Nome_Site_limpo),
    Artista_Original_limpo = gsub("\\s*\\([^\\)]+\\)", "", Artista_Original_limpo),
    Artista_Site_limpo = gsub("\\s*\\([^\\)]+\\)", "", Artista_Site_limpo)
  ) %>%
  # Remover pontuação
  mutate(
    Nome_Original_limpo = gsub("[[:punct:]]", "", Nome_Original_limpo),
    Artista_Original_limpo = gsub("[[:punct:]]", "", Artista_Original_limpo),
    Nome_Site_limpo = gsub("[[:punct:]]", "", Nome_Site_limpo),
    Artista_Site_limpo = gsub("[[:punct:]]", "", Artista_Site_limpo)
  ) %>%
  # Remover acentuação (aplicando a função auxiliar)
  mutate(
    Nome_Original_limpo = sapply(Nome_Original_limpo, remove_acentos),
    Artista_Original_limpo = sapply(Artista_Original_limpo, remove_acentos),
    Nome_Site_limpo = sapply(Nome_Site_limpo, remove_acentos),
    Artista_Site_limpo = sapply(Artista_Site_limpo, remove_acentos)
  )

# --- 2. Comparação e criação dos novos dataframes com base em df_final_total_combinado_simples_limpo ---

# Dataframe onde os nomes das músicas são iguais, mas os nomes dos artistas são diferentes
df_musicas_iguais_artistas_diferentes_final <- df_final_total_combinado_simples_limpo %>%
  filter(
    Nome_Original_limpo == Nome_Site_limpo,
    Artista_Original_limpo != Artista_Site_limpo
  ) %>%
  # Seleciona as colunas originais e as novas colunas limpas relevantes
  select(Codigo, Nome_Original, Artista_Original, Nome_Site, Artista_Site, 
         Nome_Original_limpo, Artista_Original_limpo, Nome_Site_limpo, Artista_Site_limpo, 
         Letra_Site, Ano, everything())

# Dataframe onde tanto os nomes das músicas quanto os nomes dos artistas são diferentes
df_musicas_e_artistas_diferentes_final <- df_final_total_combinado_simples_limpo %>%
  filter(
    Nome_Original_limpo != Nome_Site_limpo,
    Artista_Original_limpo != Artista_Site_limpo
  ) %>%
  # Seleciona as colunas originais e as novas colunas limpas relevantes
  select(Codigo, Nome_Original, Artista_Original, Nome_Site, Artista_Site, 
         Nome_Original_limpo, Artista_Original_limpo, Nome_Site_limpo, Artista_Site_limpo, 
         Letra_Site, Ano, everything())

# Dataframe onde as músicas e artistas são iguais
df_musicas_e_artistas_iguais_final <- df_final_total_combinado_simples_limpo %>%
  filter(
    Nome_Original_limpo == Nome_Site_limpo,
    Artista_Original_limpo == Artista_Site_limpo
  ) %>%
  # Seleciona as colunas originais e as novas colunas limpas relevantes
  select(Codigo, Nome_Original, Artista_Original, Nome_Site, Artista_Site, 
         Nome_Original_limpo, Artista_Original_limpo, Nome_Site_limpo, Artista_Site_limpo, 
         Letra_Site, Ano, everything())

# Dataframe de músicas com NAs (informações do site não encontradas)
df_dados_site_nao_encontrados_final <- df_final_total_combinado_simples_limpo %>%
  filter(is.na(Nome_Site) | is.na(Artista_Site) | is.na(Letra_Site)) %>%
  # Seleciona as colunas originais e as novas colunas limpas relevantes
  select(Codigo, Nome_Original, Artista_Original, Nome_Site, Artista_Site, 
         Nome_Original_limpo, Artista_Original_limpo, Nome_Site_limpo, Artista_Site_limpo, 
         Letra_Site, Ano, everything())


# --- Exibir os primeiros resultados dos novos dataframes ---
print("Dataframe - Músicas Iguais, Artistas Diferentes (Final):")
View(df_musicas_iguais_artistas_diferentes_final)
dim(df_musicas_iguais_artistas_diferentes_final)

print("Dataframe - Músicas e Artistas Diferentes (Final):")
View(df_musicas_e_artistas_diferentes_final)
dim(df_musicas_e_artistas_diferentes_final)

print("Dataframe - Músicas e Artistas Iguais (Final):")
View(df_musicas_e_artistas_iguais_final)
dim(df_musicas_e_artistas_iguais_final)

print("Dataframe - Dados do Site NÃO Encontrados (Final):")
View(df_dados_site_nao_encontrados_final)
dim(df_dados_site_nao_encontrados_final)

# Opcional: Salvar os novos dataframes
# write.csv(df_musicas_iguais_artistas_diferentes_final, "musicas_iguais_artistas_diferentes_final.csv", row.names = FALSE, fileEncoding = "UTF-8")
# write.csv(df_musicas_e_artistas_diferentes_final, "musicas_e_artistas_diferentes_final.csv", row.names = FALSE, fileEncoding = "UTF-8")
# write.csv(df_musicas_e_artistas_iguais_final, "musicas_e_artistas_iguais_final.csv", row.names = FALSE, fileEncoding = "UTF-8")
# write.csv(df_dados_site_nao_encontrados_final, "dados_site_nao_encontrados_final.csv", row.names = FALSE, fileEncoding = "UTF-8")