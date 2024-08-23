library(rvest)
library(dplyr)
library(stringr)



url <- "https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_75.html"
pagina <- read_html(url)


nomes_musicas <- pagina %>%
  html_nodes("table tr td:nth-child(2) font") %>%
  html_text(trim = TRUE)

nomes_musicas <- str_replace_all(nomes_musicas, "\\s+\\n\\s*", " ")


musicas_artistas <- str_split_fixed(nomes_musicas, " - ", 2)
colnames(musicas_artistas) <- c("Nome", "Artista")


musicas_artistas_df <- as.data.frame(musicas_artistas) %>%
  dplyr::mutate(across(everything(), stringr::str_squish))


print(musicas_artistas_df)

