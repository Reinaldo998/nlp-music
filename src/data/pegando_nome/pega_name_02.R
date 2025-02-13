library(rvest)
library(stringr)

# URL da página para o ano de 2002
url_2002 <- "https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_02.html"

# Ler o HTML da página
pagina_2002 <- read_html(url_2002)

# Seletores CSS para pegar as músicas e artistas
musicas_2002 <- pagina_2002 %>%
  html_nodes("td") %>%
  html_text(trim = TRUE)

# Remover as linhas desnecessárias (Top Hits, Veja, etc.) e as linhas com números
musicas_filtradas_2002 <- musicas_2002[musicas_2002 != "" & !grepl("^\\d+$", musicas_2002) & !grepl("Top Hits|Veja", musicas_2002)]

# Separando as músicas e artistas (ajuste no separador)
musicas_split_2002 <- str_split(musicas_filtradas_2002, "-|-\n", simplify = TRUE)

# Criando o data.frame para o ano de 2002
musicas_df_2002 <- data.frame(
  Nome = trimws(musicas_split_2002[, 1]),  # Remover espaços extras no nome da música
  Artista = trimws(musicas_split_2002[, 2]),  # Remover espaços extras no nome do artista
  stringsAsFactors = FALSE
)

# Remover o '\n' indesejado no nome do artista
musicas_df_2002$Artista <- gsub("\n", "", musicas_df_2002$Artista)

# Limpar espaços extras no nome do artista
musicas_df_2002$Artista <- str_squish(musicas_df_2002$Artista)

# Limpar espaços extras no nome da música
musicas_df_2002$Nome <- str_squish(musicas_df_2002$Nome)

# Adicionando a coluna do ano
musicas_df_2002$Ano <- 2002

# Criando o identificador com o formato "Ano + classificação"
musicas_df_2002$Identificador <- sprintf("%02d%02d", 2002 %% 100, 1:nrow(musicas_df_2002))

# Verificando o resultado
#print(musicas_df_2002)

# Exibindo o data.frame final
View(musicas_df_2002)

#write.csv(musicas_df_2002, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_02.csv", row.names = FALSE)
