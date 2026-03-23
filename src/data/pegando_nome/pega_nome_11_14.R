library(rvest)
library(dplyr)

# Função para extrair dados de um ano específico
extrair_musicas_por_ano <- function(ano) {
  # Formatar a URL com o ano
  ano_formatado <- substr(ano, 3, 4)
  url <- paste0("https://www.mofolandia.com.br/mofolandia_nova/musica_tophits_", ano_formatado, ".html")
  
  # Ler a página
  pagina <- read_html(url)
  
  # Verificar todas as tabelas na página
  tabelas <- pagina %>% html_nodes("table")
  
  # Extrair o conteúdo da primeira tabela
  tabela <- tabelas[[1]]
  
  # Usar html_nodes para capturar as linhas da tabela
  linhas <- tabela %>% html_nodes("tr")
  
  # Inicializar listas para armazenar os dados
  musicas <- c()
  artistas <- c()
  
  # Iterar sobre as linhas da tabela
  for (linha in linhas[-1]) {  # Ignorar o cabeçalho
    colunas <- linha %>% html_nodes("td") %>% html_text(trim = TRUE)
    
    # Verificar se a linha contém as informações necessárias
    if (length(colunas) >= 4) {  # Precisamos da música e do artista
      musicas <- c(musicas, colunas[3])  # Nome da música
      artistas <- c(artistas, colunas[4])  # Nome do artista
    }
  }
  
  # Criar um data.frame com Música, Artista e Ano
  dados_musicas <- data.frame(
    Música = musicas,
    Artista = artistas,
    Ano = rep(ano, length(musicas)),  # Adicionando o ano correspondente
    stringsAsFactors = FALSE
  )
  
  return(dados_musicas)
}

# Anos desejados
anos <- 2011:2014

# Coletar todos os dados em um data.frame
todos_dados <- bind_rows(lapply(anos, extrair_musicas_por_ano))

# Remover as linhas de cabeçalho (se necessário)
for (ano in anos) {
  if (ano == 2011) {
    # Para 2011, remove apenas a primeira linha
    todos_dados <- todos_dados[-which(todos_dados$Ano == 2011)[1], ]
  } else {
    # Para os demais anos, remove as duas primeiras linhas
    todos_dados <- todos_dados[-which(todos_dados$Ano == ano)[1:2], ]
  }
}

# Resetar os índices das linhas
rownames(todos_dados) <- NULL

# Adicionar a coluna 'Codigo' com a formatação de ano + número sequencial
todos_dados <- todos_dados %>%
  group_by(Ano) %>%
  mutate(Codigo = paste0(substr(Ano, 3, 4), sprintf("%02d", row_number()))) %>%
  ungroup()

# Exibir o resultado final
#print(todos_dados)
View(todos_dados)

#write.csv(todos_dados, "C:\\Users\\reina\\OneDrive\\Área de Trabalho\\Mestrado\\Codigos\\extraindo_musica\\data\\musicas_completas_11_14.csv", row.names = FALSE)


