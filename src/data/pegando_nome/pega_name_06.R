# Carregar os pacotes necessários
library(rvest)
library(stringr)

# Carregar o HTML da página
url <- "https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_06.html"
pagina <- read_html(url)

# Extrair o texto que contém as músicas e artistas
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
    # Remover espaços desnecessários
    musica_clean <- str_squish(partes[1])
    artista_clean <- str_squish(partes[2])
    
    # Adicionar ao data.frame, com o ano de 2006
    dados_musicas <- rbind(dados_musicas, data.frame(Musica = musica_clean, Artista = artista_clean, Ano = 2006, stringsAsFactors = FALSE))
  }
}

# Adicionar a coluna "Codigo" com a classificação da música para o ano de 2006
dados_musicas$Codigo <- paste0("06", sprintf("%02d", 1:nrow(dados_musicas)))

# Visualizar o resultado
View(dados_musicas)

#write.csv(dados_musicas, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_06.csv", row.names = FALSE)

