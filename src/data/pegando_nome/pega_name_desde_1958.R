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

# Loop para os anos de 1958 a 1999, pulando 1991
for (ano in 58:99) {
  if (ano != 91) {  # Pular o ano 1991
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
}

# Inclui o ano de 2000 no intervalo
musicas_2000 <- extrair_musicas_por_ano("00")
if (nrow(musicas_2000) > 0) {
  musicas_2000$Ano <- "2000"
  musicas_2000 <- musicas_2000 %>% select(Nome, Artista, Ano, Codigo)  # Reordena as colunas
  musicas_completas <- rbind(musicas_completas, musicas_2000)
} else {
  message("Nenhuma música encontrada para o ano: 2000")
}

# Visualiza o resultado final com a nova coluna "Codigo" como última coluna
#print(musicas_completas)
musicas_completas[1313, "Artista"] <- "Gal Costa"
musicas_completas[1313, "Nome"] <-"Como Dois e Dois"
musicas_completas <- musicas_completas[-c(1314, 1315), ]
View(musicas_completas)

# Salva o resultado em um arquivo CSV
#write.csv(musicas_completas, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_58_2000.csv", row.names = FALSE)
