library(rvest)
library(stringr)

# URL da página para o ano de 2003
url_2003 <- "https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_03.html"

# Ler o HTML da página
pagina_2003 <- read_html(url_2003)

# Seletores CSS para pegar as músicas e artistas
musicas_2003 <- pagina_2003 %>%
  html_nodes("td") %>%
  html_text(trim = TRUE)

# Remover as linhas desnecessárias (Top Hits, Veja, etc.) e as linhas com números
musicas_filtradas_2003 <- musicas_2003[musicas_2003 != "" & !grepl("^\\d+$", musicas_2003) & !grepl("Top Hits|Veja", musicas_2003)]

# Separando as músicas e artistas (ajuste no separador)
musicas_split_2003 <- str_split(musicas_filtradas_2003, "-|-\n", simplify = TRUE)

# Criando o data.frame para o ano de 2003
musicas_df_2003 <- data.frame(
  Nome = trimws(musicas_split_2003[, 1]),  # Remover espaços extras no nome da música
  Artista = trimws(musicas_split_2003[, 2]),  # Remover espaços extras no nome do artista
  stringsAsFactors = FALSE
)

# Remover o '\n' indesejado no nome do artista
musicas_df_2003$Artista <- gsub("\n", "", musicas_df_2003$Artista)

# Limpar espaços extras no nome do artista
musicas_df_2003$Artista <- str_squish(musicas_df_2003$Artista)

# Limpar espaços extras no nome da música
musicas_df_2003$Nome <- str_squish(musicas_df_2003$Nome)

# Adicionando a coluna do ano
musicas_df_2003$Ano <- 2003

# Criando o identificador com o formato "Ano + classificação"
musicas_df_2003$Identificador <- sprintf("%02d%02d", 2003 %% 100, 1:nrow(musicas_df_2003))

# Exibindo o data.frame final
View(musicas_df_2003)

#write.csv(musicas_df_2003, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_03.csv", row.names = FALSE)
