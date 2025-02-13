library(rvest)
library(stringr)

# URL da página para o ano de 2004
url_2004 <- "https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_04.html"

# Ler o HTML da página
pagina_2004 <- read_html(url_2004)

# Seletores CSS para pegar as músicas e artistas
musicas_2004 <- pagina_2004 %>%
  html_nodes("td") %>%
  html_text(trim = TRUE)

# Remover as linhas desnecessárias (Top Hits, Veja, etc.) e as linhas com números
musicas_filtradas_2004 <- musicas_2004[musicas_2004 != "" & !grepl("^\\d+$", musicas_2004) & !grepl("Top Hits|Veja", musicas_2004)]

# Separando as músicas e artistas (ajuste no separador)
musicas_split_2004 <- str_split(musicas_filtradas_2004, "-|-\n", simplify = TRUE)

# Criando o data.frame para o ano de 2004
musicas_df_2004 <- data.frame(
  Nome = trimws(musicas_split_2004[, 1]),  # Remover espaços extras no nome da música
  Artista = trimws(musicas_split_2004[, 2]),  # Remover espaços extras no nome do artista
  stringsAsFactors = FALSE
)

# Remover o '\n' indesejado no nome do artista
musicas_df_2004$Artista <- gsub("\n", "", musicas_df_2004$Artista)

# Limpar espaços extras no nome do artista
musicas_df_2004$Artista <- str_squish(musicas_df_2004$Artista)

# Adicionando a coluna do ano
musicas_df_2004$Ano <- 2004

# Criando o identificador com o formato "Ano + classificação"
musicas_df_2004$Identificador <- sprintf("%02d%02d", 2004 %% 100, 1:nrow(musicas_df_2004))


# Exibindo o data.frame final
View(musicas_df_2004)

#write.csv(musicas_df_2004, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_04.csv", row.names = FALSE)
