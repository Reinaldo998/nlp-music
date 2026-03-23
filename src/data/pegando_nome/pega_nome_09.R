# Carregar os pacotes necessários
library(httr)
library(rvest)
library(dplyr)
library(stringr)

# Definindo o ano e a URL
ano <- 2009
url <- "https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_09.html"

# Fazendo a requisição HTTP sem verificação SSL
pagina_raw <- GET(url, config(ssl_verifypeer = FALSE))

# Lendo o conteúdo da página como HTML
pagina <- read_html(content(pagina_raw, as = "text"))

# Extraindo as músicas e artistas diretamente da tabela
musicas_nodes <- pagina %>%
  html_nodes("table tr")  # Captura todas as linhas da tabela

# Inicializando listas para armazenar os nomes das músicas e artistas
nomes <- c()
artistas <- c()

# Percorrendo cada linha capturada
for (node in musicas_nodes) {
  # Extraindo o texto das colunas e removendo os espaços desnecessários
  colunas <- node %>% html_nodes("td") %>% html_text(trim = TRUE)
  
  # Verificando se a linha contém informações sobre a música e o artista
  if (length(colunas) >= 4) {  # Certifique-se de que há pelo menos 4 colunas
    # Nome da música geralmente na terceira coluna e artista na quarta
    nome_musica <- colunas[3]   # Nome da música
    artista_musica <- colunas[4] # Artista (quarta coluna)
    
    # Adiciona à lista apenas se ambas as colunas estiverem preenchidas
    if (nome_musica != "" && artista_musica != "") {
      nomes <- c(nomes, nome_musica)
      artistas <- c(artistas, artista_musica)
    }
  }
}

# Estruturando em um data.frame
musicas <- data.frame(
  Nome = str_trim(nomes),
  Artista = str_trim(artistas),
  Ano = ano,
  stringsAsFactors = FALSE
)

# Adicionar a coluna "Codigo" com a classificação da música para o ano de 2009
musicas$Codigo <- paste0("09", sprintf("%02d", 1:nrow(musicas)))

# Exibindo o data.frame resultante
View(musicas)

#write.csv(musicas, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_09.csv", row.names = FALSE)

