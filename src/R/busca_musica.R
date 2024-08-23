library(rvest)
library(dplyr)
library(stringr)

nome_musica <- "garoto de pobre - Geraldo filme"

url_pesquisa <- paste0("https://www.letras.mus.br/?q=", URLencode(nome_musica))
pagina_pesquisa <- read_html(url_pesquisa)

url_resultados <- url_pesquisa

print(paste("URL da página de resultados:", url_resultados))
