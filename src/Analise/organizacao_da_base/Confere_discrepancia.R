library(dplyr) 
library(stringr)
library(readr)
library(stringi)  # Para remover acentos

# Definir a faixa de anos relevante
ano_min <- 1958
ano_max <- 2017
ano_max_json <- 1975  # Último ano disponível nos JSONs

# Filtrar o CSV pela coluna 'Ano' antes da junção
df_csv <- df_csv %>%
  filter(Ano >= ano_min & Ano <= ano_max)

# Ler os dados dos arquivos JSON (já realizado no código anterior)
df_json <- bind_rows(lapply(json_files, ler_json))

# Unir os dados do CSV com os JSONs pelo ID
df_final <- left_join(df_csv, df_json, by = c("Codigo" = "ID"))

# Função para remover parênteses, acentuação, pontuação e comparar de forma case-insensitiva
remove_parentheses_and_normalize <- function(text) {
  # Remove os parênteses
  text <- str_trim(str_replace_all(text, "\\s*\\(.*?\\)\\s*", ""))
  
  # Remove a acentuação
  text <- stri_trans_general(text, "latin-ascii")  # Remove acentuação
  
  # Remove pontuação (qualquer caractere não alfanumérico)
  text <- str_replace_all(text, "[[:punct:]]", "")  # Remove pontuação
  
  return(tolower(text))  # Retorna o texto em minúsculas
}

# Criar colunas sem parênteses, sem acentuação, sem pontuação e convertidas para minúsculas
df_final <- df_final %>%
  mutate(Nome_Limpo = remove_parentheses_and_normalize(Nome.x),  # Nome do CSV
         Nome_Json_Limpo = remove_parentheses_and_normalize(Nome.y),  # Nome do JSON
         Artista_Limpo = remove_parentheses_and_normalize(Artista.x),  # Artista do CSV
         Artista_Json_Limpo = remove_parentheses_and_normalize(Artista.y))  # Artista do JSON

# Filtrar as discrepâncias (casos onde o nome do CSV é diferente do JSON ou o nome do JSON é NA)
df_diferentes <- df_final %>%
  filter((Nome_Limpo != Nome_Json_Limpo | is.na(Nome.y)) & Ano <= ano_max_json)  # Garante que pegamos só até 1975

# Filtrar os casos onde o nome do JSON é NA, mas apenas para anos <= 1975
df_na_json <- df_final %>%
  filter(is.na(Nome.y) & Ano <= ano_max_json)

# Criar df_diferentes_2 apenas com os casos onde o nome da música **e** do artista são distintos
df_diferentes_2 <- df_diferentes %>%
  filter(Artista_Limpo != Artista_Json_Limpo | is.na(Artista.y))

# Exibir os resultados
View(df_diferentes)
View(df_na_json)
View(df_diferentes_2)


n_precisa<-c(163,174,200,206,216,246,267,164,275,301,312,315,320,321,323,327,328,342,357,361,363,367,421,424,449,466)
