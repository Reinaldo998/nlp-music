# Bibliotecas necessárias
library(rvest)
library(dplyr)
library(stringr)

# URL da página
url <- "https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_13.html"

# Lendo o conteúdo da página
pagina <- read_html(url)

# Extraindo o texto completo do corpo da página
texto_completo <- pagina %>%
  html_nodes("body") %>%
  html_text(trim = TRUE)

# Dividindo o texto em linhas
linhas <- str_split(texto_completo, "\n")[[1]] %>%
  str_trim() %>%  # Remove espaços desnecessários
  .[. != ""]      # Remove linhas vazias

# Inicializa as listas para armazenar os dados
musicas <- c()
artistas <- c()

# Itera pelas linhas e processa a cada duas linhas
for (i in 1:(length(linhas) - 1)) {
  # Verifica se a linha atual contém um número (indicando a posição da música)
  if (str_detect(linhas[i], "^\\d+$")) {
    musica <- linhas[i + 1]  # A próxima linha é o nome da música
    artista <- linhas[i + 2]  # A linha depois da próxima é o nome do artista
    
    # Adiciona à lista de músicas e artistas
    musicas <- c(musicas, musica)
    artistas <- c(artistas, artista)
  }
}

# Garantir que os nomes e artistas estão nas colunas corretas
for (i in 1:length(musicas)) {
  if (str_detect(musicas[i], "^[a-z]") & str_detect(artistas[i], "^[A-Z]")) {
    # Se o nome da música começar com letra minúscula e o artista com maiúscula, invertemos
    temp <- musicas[i]
    musicas[i] <- artistas[i]
    artistas[i] <- temp
  }
}

# Criando o data frame
dados <- data.frame(
  Nome = str_squish(musicas),    # Nome da música
  Artista = str_squish(artistas),# Nome do artista
  stringsAsFactors = FALSE
)

# Adicionar coluna do ano
dados$Ano <- 2013

# Adicionar coluna de código único
dados$Codigo <- paste0("13", sprintf("%02d", 1:nrow(dados)))

# Visualizar o resultado
View(dados)

# Salvar em um arquivo CSV
#write.csv(dados, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_2013.csv", row.names = FALSE)
