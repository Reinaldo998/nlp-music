library(rvest)
library(stringr)

# URL da página para o ano de 1991
url_1991 <- "https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_91.html"

# Ler o HTML da página
pagina_1991 <- read_html(url_1991)

# Seletores CSS para pegar as músicas e artistas
musicas_1991 <- pagina_1991 %>%
  html_nodes("td") %>%
  html_text(trim = TRUE)

# Remover as linhas desnecessárias (Top Hits, Veja, etc.) e as linhas com números
musicas_filtradas_1991 <- musicas_1991[musicas_1991 != "" & 
                                         !grepl("^\\d+$", musicas_1991) & 
                                         !grepl("Top Hits|Veja", musicas_1991)]

# Ajustar a separação para capturar músicas separadas por "-\n" ou "-"
musicas_split_1991 <- str_split(musicas_filtradas_1991, "\\s*-\\s*\\n?|\\s*-\\s*", simplify = TRUE)

# Criando o data.frame para o ano de 1991
musicas_df_1991 <- data.frame(
  Nome = trimws(musicas_split_1991[, 1]),  # Remover espaços extras no nome da música
  Artista = trimws(musicas_split_1991[, 2]),  # Remover espaços extras no nome do artista
  stringsAsFactors = FALSE
)

# Remover o '\n' indesejado no nome do artista
musicas_df_1991$Artista <- gsub("\n", "", musicas_df_1991$Artista)

# Limpar espaços extras no nome do artista e da música
musicas_df_1991$Artista <- str_squish(musicas_df_1991$Artista)
musicas_df_1991$Nome <- str_squish(musicas_df_1991$Nome)

# Adicionando a coluna do ano
musicas_df_1991$Ano <- 1991

# Criando o identificador com o formato "Ano + classificação"
musicas_df_1991$Identificador <- sprintf("%02d%02d", 1991 %% 100, 1:nrow(musicas_df_1991))

# Exibindo o data.frame final
View(musicas_df_1991)

#write.csv(musicas_df_1991, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_91.csv", row.names = FALSE)
