library(jsonlite)
library(dplyr)
library(readr)

# Definir o 'diretório onde estão os arquivos JSON
json_dir <- '/home/daiane/Reinaldo/processed'

# Listar todos os arquivos JSON na pasta
json_files <- list.files(json_dir, pattern = "*.json", full.names = TRUE)

# Função para ler um arquivo JSON e transformá-lo em um data.frame
ler_json <- function(file) {
  dados <- fromJSON(file)
  return(data.frame(ID = as.character(dados$ID),
                    Nome = dados$Nome,
                    Artista = dados$Artista,
                    Letra = dados$Letra,
                    stringsAsFactors = FALSE))
}

# Aplicar a função a todos os arquivos JSON e combinar em um único data.frame
df_json <- bind_rows(lapply(json_files, ler_json))

# Ler o CSV original
csv_path <- '/home/daiane/Reinaldo/musicas_completas.csv'
df_csv <- read_csv(csv_path, col_types = cols(Codigo = col_character()))

# Unir os dados do CSV com os dados dos JSONs pelo ID
df_final <- left_join(df_csv, df_json, by = c("Codigo" = "ID"))
df_final<-df_final[1:1800,]
# Exibir o resultado
View(df_final)

# Se quiser salvar o resultado em um novo CSV:
#write_csv(df_final, "C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/extraindo_musica/data/musicas_completas_com_letras.csv")
