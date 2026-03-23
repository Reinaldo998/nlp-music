library(rvest)
library(stringr)

# URL da página
url <- "https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_05.html"

# Ler o HTML da página
pagina <- read_html(url)

# Seletores CSS para pegar as músicas e artistas
musicas <- pagina %>%
  html_nodes("td") %>%  
  html_text(trim = TRUE)

# Remover linhas irrelevantes ou com números sozinhos
musicas_filtradas <- musicas[musicas != "" & 
                               !grepl("Top Hits|Veja|músicas mais tocadas", musicas, ignore.case = TRUE) & 
                               !grepl("^\\d+$", musicas)]

# Separando as músicas e artistas
musicas_split <- str_split(musicas_filtradas, "-|-\n", simplify = TRUE)

# Criando o data.frame sem os números
musicas_df <- data.frame(
  Nome = trimws(musicas_split[, 1]),  # Remover espaços extras no nome da música
  Artista = trimws(musicas_split[, 2]),  # Remover espaços extras no nome do artista
  stringsAsFactors = FALSE
)

# Limpar quebras de linha e espaços adicionais no nome do artista
musicas_df$Artista <- gsub("\n", "", musicas_df$Artista)
musicas_df <- musicas_df[!is.na(musicas_df$Artista) & musicas_df$Artista != "", ]  # Remover linhas com artistas vazios
musicas_df$Artista <- str_squish(musicas_df$Artista)

# Adicionando a coluna do ano
musicas_df$Ano <- 2005

# Criando o identificador com o formato "Ano + classificação"
musicas_df$Identificador <- sprintf("%02d%02d", 2005 %% 100, 1:nrow(musicas_df))

# Exibindo o data.frame final
View(musicas_df)

#write.csv(musicas_df, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_05.csv", row.names = FALSE)
