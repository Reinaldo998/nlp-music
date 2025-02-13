library(rvest)
library(dplyr)
library(stringr)

# URL da página com as músicas de 2017
url <- "https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_17.html"

# Ler o HTML da página
pagina <- read_html(url)

# Extrair as músicas e artistas
musicas <- pagina %>%
  html_nodes("p") %>%
  html_text()

# Filtrar apenas as linhas que contêm músicas
musicas <- musicas[str_detect(musicas, "-")]

# Separar nomes das músicas e artistas, removendo o número no início
musica_artista <- str_match(musicas, "\\d+ - (.*) - (.*)")
musica <- str_trim(musica_artista[, 2])
artista <- str_trim(musica_artista[, 3])

# Criar o data.frame e remover linhas com NAs
resultado <- data.frame(Nome = musica, Artista = artista, Ano = 2017, stringsAsFactors = FALSE)
resultado <- resultado[complete.cases(resultado), ]  # Remove NAs

# Adicionar a coluna Codigo
resultado$Codigo <- paste0("17", sprintf("%02d", seq_along(resultado$Nome)))

# Exibir o resultado
#print(resultado)

View(resultado)

#write.csv(resultado, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_17.csv", row.names = FALSE)






