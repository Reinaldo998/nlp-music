library(rvest)
library(stringr)

# Carregar o HTML da página
url <- "https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_01.html"
pagina <- read_html(url)

# Extrair os parágrafos que contêm as músicas e artistas
musicas <- pagina %>% html_nodes("p") %>% html_text()

# Filtrar apenas as linhas relevantes (músicas)
musicas_filtradas <- musicas[musicas != ""]

# Criar um data.frame para armazenar as músicas, artistas e ano
dados_musicas <- data.frame(Musica = character(), Artista = character(), Ano = integer(), stringsAsFactors = FALSE)

# Separar música e artista
for (linha in musicas_filtradas) {
  # Separa usando o " - " como delimitador
  partes <- strsplit(linha, " - ")[[1]]
  if (length(partes) == 2) {
    dados_musicas <- rbind(dados_musicas, data.frame(Musica = partes[1], Artista = partes[2], Ano = 2001, stringsAsFactors = FALSE))
  }
}

# Limpar os espaços extras nos nomes das músicas e dos artistas
dados_musicas$Musica <- str_squish(dados_musicas$Musica)
dados_musicas$Artista <- str_squish(dados_musicas$Artista)

# Adicionar a coluna "Codigo" com a classificação da música para o ano de 2001
dados_musicas$Codigo <- paste0("01", sprintf("%02d", 1:nrow(dados_musicas)))

# Ver o resultado final
View(dados_musicas)

#write.csv(dados_musicas, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_01.csv", row.names = FALSE)
