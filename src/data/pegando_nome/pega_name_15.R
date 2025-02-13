library(rvest)
library(dplyr)
library(stringr)

# URL da página de 2015
url <- 'https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_15.html'

# Lendo o HTML da página
pagina <- read_html(url)

# Extraindo as músicas e artistas
musicas <- pagina %>%
  html_nodes("p") %>%        # Seleciona todos os parágrafos
  html_text() %>%           # Extrai o texto
  str_trim() %>%            # Remove espaços em branco nas extremidades
  .[!grepl("^$", .)]         # Remove linhas vazias

# Processando os dados para separar músicas e artistas
resultado <- data.frame(musica = character(), artista = character(), stringsAsFactors = FALSE)

for (musica in musicas) {
  # Separando a música e o artista
  partes <- str_split(musica, " – ", simplify = TRUE)
  if (length(partes) == 2) {
    nome_musica <- str_trim(partes[1]) %>% str_remove("^\\s*\\d+\\s*-\\s*") %>% str_squish()
    artista <- str_trim(partes[2])
    
    resultado <- rbind(resultado, data.frame(musica = nome_musica, artista = artista, stringsAsFactors = FALSE))
  }
}

# Adicionando a coluna de código
resultado$Ano <- 2015
resultado$Codigo <- paste0("15", sprintf("%02d", seq_along(resultado$musica)))

# Exibindo o resultado
View(resultado)

#write.csv(resultado, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_15.csv", row.names = FALSE)
