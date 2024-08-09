#install.packages("rvest")
#install.packages("dplyr")
#install.packages("stringr")
library(rvest)
library(dplyr)
library(stringr)

extrair_musicas_por_ano <- function(ano) {
  url <- paste0("https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_", ano, ".html")
  pagina <- read_html(url)
  
  nomes_musicas <- pagina %>%
    html_nodes("table tr td:nth-child(2) font") %>%
    html_text(trim = TRUE)
  
  nomes_musicas <- str_replace_all(nomes_musicas, "\\s+\\n\\s*", " ")
  
  
  musicas_artistas <- str_split_fixed(nomes_musicas, " - ", 2)
  colnames(musicas_artistas) <- c("Nome", "Artista")
  
  musicas_artistas <- as.data.frame(musicas_artistas) %>%
    mutate(across(everything(), str_squish))
  
  return(musicas_artistas)
}


musicas_anos_70 <- data.frame()

for (ano in 70:79) {
  musicas <- extrair_musicas_por_ano(ano)
  
  musicas$Ano <- paste0("19", ano)
  musicas_anos_70 <- rbind(musicas_anos_70, musicas)
  
}

print(musicas_anos_70)

