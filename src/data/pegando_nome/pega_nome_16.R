library(rvest)
library(dplyr)
library(stringr)

# URL da página que você deseja extrair dados
url <- 'https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_16.html'

# Lê a página
page <- read_html(url)

# Extrai o texto que contém as músicas
musicas_texto <- page %>%
  html_nodes("body") %>%
  html_text()

# Limpa e separa as músicas e artistas
musicas <- str_split(musicas_texto, "\n")[[1]] %>%
  str_trim() %>%
  .[. != ""]  # Remove strings vazias

# Divide as músicas e artistas
musicas_artistas <- str_split(musicas, " – ", simplify = TRUE)

# Cria o data.frame e organiza as colunas, removendo aspas e caracteres especiais
resultado <- data.frame(
  Nome = str_remove_all(musicas_artistas[, 2], '["“”‘’]') %>% str_trim(),  # Remove aspas (duplas, simples e variáveis)
  Artista = str_remove_all(musicas_artistas[, 1], '["“”‘’]') %>% str_trim(),  # Remove aspas do artista
  Ano = 2016,
  stringsAsFactors = FALSE
)

# Filtra linhas válidas (caso haja algum NA)
resultado <- resultado %>%
  filter(!is.na(Nome) & !is.na(Artista) & Nome != "" & Artista != "")

# Adiciona a coluna Codigo
resultado$Codigo <- paste0("16", sprintf("%02d", seq_along(resultado$Nome)))

# Exibe o resultado
#print(resultado)
View(resultado)

# Exporta para o arquivo CSV
#write.csv(resultado, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_16.csv", row.names = FALSE)
