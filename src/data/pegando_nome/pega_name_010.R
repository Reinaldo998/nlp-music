library(rvest)
library(stringr)

# URL da página para o ano de 2010
url_2010 <- "https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_10.html"

# Ler o HTML da página
pagina_2010 <- read_html(url_2010)

# Seletores CSS para capturar os textos na tabela
musicas_2010 <- pagina_2010 %>%
  html_nodes("td") %>%
  html_text(trim = TRUE)

# Remover linhas indesejadas (cabeçalhos ou vazias)
musicas_filtradas_2010 <- musicas_2010[musicas_2010 != "" & 
                                         !grepl("^\\d+$", musicas_2010) & 
                                         !grepl("Música|Intérprete|Top Hits", musicas_2010)]

# Agrupando em pares (música e artista)
musicas_pares_2010 <- matrix(musicas_filtradas_2010, ncol = 2, byrow = TRUE)

# Criar o data.frame final
musicas_df_2010 <- data.frame(
  Nome = musicas_pares_2010[, 1],
  Artista = musicas_pares_2010[, 2],
  stringsAsFactors = FALSE
)

# Limpar espaços extras e caracteres indesejados
musicas_df_2010$Nome <- str_squish(musicas_df_2010$Nome)
musicas_df_2010$Artista <- str_squish(musicas_df_2010$Artista)

# Adicionar coluna do ano
musicas_df_2010$Ano <- 2010

# Criar identificador
musicas_df_2010$Identificador <- sprintf("%02d%02d", 2010 %% 100, 1:nrow(musicas_df_2010))

# Excluir a primeira linha se necessário (após verificações)
if (nrow(musicas_df_2010) > 0 && grepl("Top Hits", musicas_df_2010$Nome[1])) {
  musicas_df_2010 <- musicas_df_2010[-1, ]
}

# Exibir o data.frame final
View(musicas_df_2010)

#write.csv(musicas_df_2010, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_10.csv", row.names = FALSE)

