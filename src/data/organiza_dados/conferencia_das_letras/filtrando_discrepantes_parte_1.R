# Supondo que seu dataframe já esteja carregado e se chame 'Data_final'
# Não é necessário criar um dataframe de exemplo se você já tem o seu carregado

library(dplyr)

# Cria uma cópia do dataframe Data_final para trabalhar, evitando alterações no original
Data_final_trabalho <- data_final_pt

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

# 1. Limpeza dos nomes das músicas e dos artistas
# Converte para minúsculas, remove espaços em branco extras, parênteses, pontuação e acentuação
Data_final_trabalho <- Data_final_trabalho %>%
  mutate(
    Nome.x_limpo = tolower(trimws(Nome.x)),
    Artista.x_limpo = tolower(trimws(Artista.x)),
    Nome.y_limpo = tolower(trimws(Nome.y)),
    Artista.y_limpo = tolower(trimws(Artista.y))
  ) %>%
  # Remover texto entre parênteses e os próprios parênteses
  mutate(
    Nome.x_limpo = gsub("\\s*\\([^\\)]+\\)", "", Nome.x_limpo),
    Nome.y_limpo = gsub("\\s*\\([^\\)]+\\)", "", Nome.y_limpo)
  ) %>%
  # Remover pontuação
  mutate(
    Nome.x_limpo = gsub("[[:punct:]]", "", Nome.x_limpo), # Remove todos os caracteres de pontuação
    Artista.x_limpo = gsub("[[:punct:]]", "", Artista.x_limpo),
    Nome.y_limpo = gsub("[[:punct:]]", "", Nome.y_limpo),
    Artista.y_limpo = gsub("[[:punct:]]", "", Artista.y_limpo)
  ) %>%
  # Remover acentuação (aplicando a função auxiliar)
  mutate(
    Nome.x_limpo = sapply(Nome.x_limpo, remove_acentos),
    Artista.x_limpo = sapply(Artista.x_limpo, remove_acentos),
    Nome.y_limpo = sapply(Nome.y_limpo, remove_acentos),
    Artista.y_limpo = sapply(Artista.y_limpo, remove_acentos)
  )

# 2. Comparação e criação dos novos dataframes

# Dataframe onde os nomes das músicas são iguais, mas os nomes dos artistas são diferentes
df_musicas_iguais_artistas_diferentes <- Data_final_trabalho %>%
  filter(
    Nome.x_limpo == Nome.y_limpo,
    Artista.x_limpo != Artista.y_limpo
  ) %>%
  # Seleciona apenas as colunas originais e as colunas limpas relevantes
  select(Nome.x, Artista.x, Nome.y, Artista.y, Nome.x_limpo, Artista.x_limpo, Nome.y_limpo, Artista.y_limpo, everything())

# Dataframe onde tanto os nomes das músicas quanto os nomes dos artistas são diferentes
df_musicas_e_artistas_diferentes <- Data_final_trabalho %>%
  filter(
    Nome.x_limpo != Nome.y_limpo,
    Artista.x_limpo != Artista.y_limpo
  ) %>%
  # Seleciona apenas as colunas originais e as colunas limpas relevantes
  select(Nome.x, Artista.x, Nome.y, Artista.y, Nome.x_limpo, Artista.x_limpo, Nome.y_limpo, Artista.y_limpo, everything())



# Exibir os primeiros resultados dos novos dataframes (opcional)
print("Dataframe - Músicas Iguais, Artistas Diferentes:")
View(df_musicas_iguais_artistas_diferentes)
dim(df_musicas_iguais_artistas_diferentes)

print("Dataframe - Músicas e Artistas Diferentes:")
View(df_musicas_e_artistas_diferentes)
dim(df_musicas_e_artistas_diferentes)

#=====================================================================================================================


# Selecionar apenas as colunas 'Nome.x' e 'Artista.x' e renomeá-las
# para 'Nome' e 'Artista' para corresponder ao que o script Python espera.
# Adicionaremos também o ID da música original, se houver uma coluna 'ID'
# no seu df_musicas_iguais_artistas_diferentes.
# Assumindo que a coluna de ID original é 'ID' no Data_final
musicas_para_python <- df_musicas_iguais_artistas_diferentes %>%
  select(Codigo = ID, Nome = Nome.x, Artista = Artista.x, Ano) # Adicionando 'Ano' se for relevante para o CSV

View(musicas_para_python)
# Salvar em um arquivo CSV. Usaremos 'musicas_discrepantes_para_scraping.csv'
# para facilitar a distinção.
write.csv(musicas_para_python, "musicas_discrepantes_para_scraping.csv", row.names = FALSE, fileEncoding = "UTF-8")

print("Arquivo 'musicas_discrepantes_para_scraping.csv' criado com sucesso!")
print(head(musicas_para_python)) # Exibe as primeiras linhas do novo CSV

###############################################################################################################################

# NOVA SEÇÃO: Preparação para o Python: Músicas e Artistas Diferentes
# Selecionar as colunas relevantes e renomeá-las para corresponder ao script Python
# Assumindo que a coluna de ID original é 'ID' no seu df_musicas_e_artistas_diferentes
musicas_artistas_diferentes_para_python <- df_musicas_e_artistas_diferentes %>%
  select(
    Codigo = ID, # Renomeia 'ID' para 'Codigo'
    Nome = Nome.x, # Pega 'Nome.x' e renomeia para 'Nome'
    Artista = Artista.x, # Pega 'Artista.x' e renomeia para 'Artista'
    Ano # Mantém a coluna 'Ano'
  )

View(musicas_artistas_diferentes_para_python)
# Salvar em um novo arquivo CSV.
# Usaremos 'musicas_artistas_diferentes_para_scraping.csv'
# para facilitar a distinção e evitar conflitos.
write.csv(musicas_artistas_diferentes_para_python, "musicas_artistas_diferentes_para_scraping.csv", row.names = FALSE, fileEncoding = "UTF-8")

print("Arquivo 'musicas_artistas_diferentes_para_scraping.csv' criado com sucesso!")
print(head(musicas_artistas_diferentes_para_python)) # Exibe as primeiras linhas do novo CSV