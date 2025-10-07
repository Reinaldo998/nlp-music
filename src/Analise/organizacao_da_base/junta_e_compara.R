# Carregar pacotes necessários
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(stringi)  # Para remover acentos

# Definir diretórios
json_dir <- "/home/daiane/Reinaldo/processed_3"
csv_path <- "/home/daiane/Reinaldo/musicas_na.csv"

# Listar arquivos JSON
json_files <- list.files(json_dir, pattern = "*.json", full.names = TRUE)

# Função para ler JSON e transformá-lo em data.frame
ler_json <- function(file) {
  dados <- fromJSON(file)
  return(data.frame(ID = as.character(dados$ID),
                    Nome = dados$Nome,
                    Artista = dados$Artista,
                    Letra = dados$Letra,
                    stringsAsFactors = FALSE))
}

# Ler os arquivos JSON e combinar em um único data.frame
df_json <- bind_rows(lapply(json_files, ler_json))

# Ler o CSV original
df_csv <- read_csv(csv_path, col_types = cols(Codigo = col_character()))

# Unir os dados do CSV e JSON pelo ID da música
df_final <- left_join(df_csv, df_json, by = c("Codigo" = "ID"))

# Função para remover parênteses, acentuação, pontuação e padronizar para minúsculas
remove_parentheses_and_normalize <- function(text) {
  text <- str_trim(str_replace_all(text, "\\s*\\(.*?\\)\\s*", ""))  # Remove parênteses
  text <- stri_trans_general(text, "latin-ascii")  # Remove acentuação
  text <- str_replace_all(text, "[[:punct:]]", "")  # Remove pontuação
  return(tolower(text))  # Converte para minúsculas
}

# Criar colunas normalizadas
df_final <- df_final %>%
  mutate(Nome_Limpo = remove_parentheses_and_normalize(Nome.x),  
         Nome_Json_Limpo = remove_parentheses_and_normalize(Nome.y),  
         Artista_Limpo = remove_parentheses_and_normalize(Artista.x),  
         Artista_Json_Limpo = remove_parentheses_and_normalize(Artista.y))

# Identificar músicas com discrepâncias entre CSV e JSON (sem restrição de ano)
df_diferentes <- df_final %>%
  filter(Nome_Limpo != Nome_Json_Limpo | is.na(Nome.y))

# Identificar músicas sem nome no JSON (sem restrição de ano)
df_na_json <- df_final %>%
  filter(is.na(Nome.y))

# Identificar músicas onde **nome e artista** são diferentes (sem restrição de ano)
df_diferentes_2 <- df_diferentes %>%
  filter(Artista_Limpo != Artista_Json_Limpo | is.na(Artista.y))

# Exibir resultados
View(df_final)  
View(df_diferentes)
View(df_na_json)
View(df_diferentes_2)

nrow(df_final)
nrow(df_diferentes_2)
nrow(df_na_json)

nrow(df_diferentes_2)/nrow(df_final)
nrow(df_na_json)/nrow(df_final)

# (Opcional) Salvar os arquivos CSV com os resultados
#write_csv(df_final, "C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/extraindo_musica/data/musicas_completas_com_letras.csv")
#write_csv(df_diferentes, "C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/extraindo_musica/data/musicas_diferentes.csv")
#write_csv(df_na_json, "C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/extraindo_musica/data/musicas_sem_json.csv")
#write_csv(df_diferentes_2, "C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/extraindo_musica/data/musicas_diferentes_artistas.csv")
#