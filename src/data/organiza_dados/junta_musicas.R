library(jsonlite)
library(dplyr)
library(stringr)
library(stringi)  # Para remover acentos

# Defina o caminho da pasta onde estão os arquivos JSON
pasta <- "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\processed"

# Lista todos os arquivos JSON na pasta
arquivos_json <- list.files(pasta, pattern = "*.json", full.names = TRUE)

# Função para ler um JSON e convertê-lo para data.frame
ler_json_para_df <- function(arquivo) {
  tryCatch({
    conteudo <- fromJSON(arquivo, flatten = TRUE)
    as.data.frame(conteudo)
  }, error = function(e) {
    message(paste("Erro ao ler:", arquivo))
    return(NULL)
  })
}

# Ler todos os arquivos JSON e combinar em um único data.frame
df_final <- bind_rows(lapply(arquivos_json, ler_json_para_df))

# Exibir as primeiras linhas do data.frame
View(df_final)
write_csv(df_final, "C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/data/processed.csv")

###############################################################################################################

library(readr)
processed_1 <- read_csv("C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/data/processed_1.csv")
View(processed_1)

processed_2 <- read_csv("C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/data/processed_2.csv")
View(processed_2)

processed_3 <- read_csv("C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/data/processed_3.csv")
View(processed_3)

processed <- read_csv("C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/data/processed.csv")
View(processed)

musicas_completas <- read_csv("C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/extraindo_musica/data/musicas_completas.csv")
View(musicas_completas)
colnames(musicas_completas)<-c("Nome","Artista","Ano","ID")

###################################################################################
library(dplyr)
result32 <- anti_join(processed_2, processed_3, by = "ID")
result23<-rbind(processed_3,result32)
View(result32)

result21 <- anti_join(processed_1, result23, by = "ID")
result12<-rbind(result23,result21)
View(result21)

result10 <- anti_join(processed, result12, by = "ID")
letras_c<-rbind(result12,result10)
View(letras_c)

merged_df <- full_join(musicas_completas, letras_c, by = "ID")
View(merged_df)

write_csv(merged_df, "C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/data/data_final.csv")



###############################################################################################
df_final <- left_join(musicas_completas, letras_c, by = "ID")
View(df_final)
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
nrow(df_diferentes)
nrow(df_na_json)

nrow(df_diferentes_2)/nrow(df_final)
nrow(df_na_json)/nrow(df_final)

# (Opcional) Salvar os arquivos CSV com os resultados
#write_csv(df_final, "C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/extraindo_musica/data/musicas_completas_com_letras.csv")
#write_csv(df_diferentes, "C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/extraindo_musica/data/musicas_diferentes.csv")
#write_csv(df_na_json, "C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/extraindo_musica/data/musicas_sem_json.csv")
#write_csv(df_diferentes_2, "C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/extraindo_musica/data/musicas_diferentes_artistas.csv")
#