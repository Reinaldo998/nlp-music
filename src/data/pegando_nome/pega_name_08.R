library(rvest)
library(dplyr)

# URL para o ano de 2008
url_2008 <- "https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_08.html"

# Carregar a página HTML
page_2008 <- read_html(url_2008)

# Extrair o texto da página
text_2008 <- html_text(page_2008)

# Filtrar o conteúdo das músicas e artistas
musicas_2008 <- unlist(str_extract_all(text_2008, "(\\d+\\s+[^\n]+)\\s+([^\n]+)"))


# Filtrar os dados para remover a primeira linha
musicas_2008_clean <- musicas_2008[-1]

# Usar expressões regulares para separar as músicas e artistas
musicas_2008_split <- strsplit(musicas_2008_clean, "\\n\\s*")

# Criar um data.frame com duas colunas: Música e Artista
musicas_2008_df <- data.frame(
  Musica = sapply(musicas_2008_split, function(x) x[2]),
  Artista = sapply(musicas_2008_split, function(x) x[3]),
  stringsAsFactors = FALSE
)

# Adicionar a coluna "Ano" com o valor 2008
musicas_2008_df$Ano <- 2008

# Adicionar a coluna "Identificador"
musicas_2008_df$Identificador <- sprintf("08%02d", 1:nrow(musicas_2008_df))

musicas_2008_df <- musicas_2008_df[-c(101), ]


# Exibir o data.frame resultante
View(musicas_2008_df)

#write.csv(musicas_2008_df, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_08.csv", row.names = FALSE)



