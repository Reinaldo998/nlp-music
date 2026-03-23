# Bibliotecas necessárias
library(rvest)
library(dplyr)
library(stringr)

# Função para extrair músicas por ano com código
extrair_musicas_por_ano <- function(ano) {
  # Construção da URL
  url <- paste0("https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_", ano, ".html")
  pagina <- read_html(url)
  
  # Extração dos nomes das músicas
  nomes_musicas <- pagina %>%
    html_nodes("table tr td:nth-child(2) font") %>%
    html_text(trim = TRUE)
  
  # Limpeza do texto extraído
  nomes_musicas <- str_replace_all(nomes_musicas, "\\s+\\n\\s*", " ")
  
  # Separação do nome da música e do artista
  musicas_artistas <- str_split_fixed(nomes_musicas, " - ", 2)
  colnames(musicas_artistas) <- c("Nome", "Artista")
  
  # Transformação em data.frame e limpeza de espaços extras
  musicas_artistas <- as.data.frame(musicas_artistas) %>%
    mutate(across(everything(), str_squish))
  
  # Verifica se o data.frame não está vazio antes de adicionar a coluna Código
  if (nrow(musicas_artistas) > 0) {
    musicas_artistas$Codigo <- paste0(ano, sprintf("%02d", 1:nrow(musicas_artistas)))
  }
  
  return(musicas_artistas)
}

# Inicializa o data.frame vazio
musicas_completas <- data.frame()

# Loop para os anos de 1970 a 1999
for (ano in 58:99) {
  musicas <- extrair_musicas_por_ano(ano)
  
  # Verifica se o data.frame não está vazio antes de adicionar o ano
  if (nrow(musicas) > 0) {
    musicas$Ano <- paste0("19", ano)
    musicas <- musicas %>% select(Nome, Artista, Ano, Codigo)  # Reordena as colunas
    musicas_completas <- rbind(musicas_completas, musicas)
  } else {
    message(paste("Nenhuma música encontrada para o ano:", paste0("19", ano)))
  }
}

# Loop para os anos de 2000 a 2017
for (ano in 0:17) {
  ano_formatado <- sprintf("%02d", ano)
  musicas <- extrair_musicas_por_ano(ano_formatado)
  
  # Verifica se o data.frame não está vazio antes de adicionar o ano
  if (nrow(musicas) > 0) {
    musicas$Ano <- paste0("20", ano_formatado)
    musicas <- musicas %>% select(Nome, Artista, Ano, Codigo)  # Reordena as colunas
    musicas_completas <- rbind(musicas_completas, musicas)
  } else {
    message(paste("Nenhuma música encontrada para o ano:", paste0("20", ano_formatado)))
  }
}

# Visualiza o resultado final com a nova coluna "Codigo" como última coluna
print(musicas_completas)
View(musicas_completas)

#write.csv(musicas_completas, "C:\Users\reina\OneDrive\Área de Trabalho\Mestrado\Codigos\extraindo_musica\data\musicas_completas.csv", row.names = FALSE)

#write.csv(musicas_completas, "C:/Users/reina/OneDrive/Área de Trabalho/Mestrado/Codigos/extraindo_musica/data/musicas_completas.csv", row.names = FALSE)
write.csv(musicas_completas, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_58.csv", row.names = FALSE)

