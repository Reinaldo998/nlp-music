# Criação do corpus_decada
corpus_decada <- data_final %>%
  mutate(Decada = as.character(floor(Ano / 10) * 10)) %>% # Padroniza a década como texto
  group_by(Decada) %>%
  summarise(texto = paste(Letra, collapse = " ")) %>%
  ungroup()

# Data frame vazio para armazenar resultados
top_palavras_decada <- data.frame(Decada = character(),
                                  Palavra = character(),
                                  Frequencia = integer(),
                                  stringsAsFactors = FALSE)

# Loop por década
for(i in unique(corpus_decada$Decada)) {
  # Filtra músicas da década
  musica_decada <- corpus_decada %>%
    filter(Decada == i)
  
  # Cria o corpus
  corpus <- VCorpus(VectorSource(musica_decada$texto))
  
  # Limpeza do texto
  corpus_clean <- corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("pt")) %>%
    tm_map(stripWhitespace)
  
  # Matriz de termos
  dtm <- DocumentTermMatrix(corpus_clean)
  
  # Frequência das palavras
  freq <- colSums(as.matrix(dtm))
  freq_df <- data.frame(Palavra = names(freq), Frequencia = freq, row.names = NULL)
  
  # Seleciona as 100 palavras mais frequentes
  top_100 <- freq_df %>%
    arrange(desc(Frequencia)) %>%
    slice_head(n = 100)
  
  # Adiciona a coluna de década
  top_100$Decada <- i
  
  # Adiciona ao data frame final
  top_palavras_decada <- bind_rows(top_palavras_decada, top_100)
}

# Visualiza o resultado
View(top_palavras_decada)
############################################################################################

# Filtra apenas músicas em português
data_final_pt <- Data_Final %>%
  filter(idiomas == "pt")


data_final_pt <- Data_Final %>%
  left_join(N_palavras %>% select(ID, Idioma), by = "ID") %>%
  filter(Idioma == "pt")


# Criação do corpus_decada
corpus_decada <- data_final_pt %>%
  mutate(Decada = as.character(floor(Ano / 10) * 10)) %>% # Cria década como texto
  group_by(Decada) %>%
  summarise(texto = paste(Letra, collapse = " ")) %>%
  ungroup()

# Lista para armazenar palavras por década
palavras_por_decada <- list()

# Loop por década
for(i in unique(corpus_decada$Decada)) {
  # Filtra músicas da década
  musica_decada <- corpus_decada %>%
    filter(Decada == i)
  
  # Cria o corpus
  corpus <- VCorpus(VectorSource(musica_decada$texto))
  
  # Limpeza do texto
  corpus_clean <- corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("pt")) %>%
    tm_map(stripWhitespace)
  
  # Matriz de termos
  dtm <- DocumentTermMatrix(corpus_clean, control = list(stopwords = TRUE))
  
  # Frequência das palavras
  freq <- colSums(as.matrix(dtm))
  freq_df <- data.frame(Palavra = names(freq), Frequencia = freq, row.names = NULL)
  
  # Seleciona as 100 palavras mais frequentes
  top_100 <- freq_df %>%
    arrange(desc(Frequencia)) %>%
    slice_head(n = 100)
  
  # Armazena só as palavras
  palavras_por_decada[[i]] <- top_100$Palavra
}

# Junta as palavras em colunas por década, ajustando o tamanho para 100 linhas
max_length <- max(sapply(palavras_por_decada, length))
top_palavras_decada1 <- do.call(cbind, lapply(palavras_por_decada, function(x) {
  length(x) <- max_length
  return(x)
}))

# Ajusta os nomes das colunas (décadas)
colnames(top_palavras_decada1) <- names(palavras_por_decada)

# Converte para data.frame
top_palavras_decada1 <- as.data.frame(top_palavras_decada1)

# Visualiza o resultado
View(top_palavras_decada1)

##################################################################
# Supondo que os dados já estejam com a coluna "Decada" criada
# Filtra os dados de músicas em português
dados_portugues <- data_final %>%
  filter(idiomas == "pt")

# Cria a coluna de década
dados_portugues <- dados_portugues %>%
  mutate(Decada = paste0(substr(Ano, 1, 3), "0"))

# Contar o número de músicas por artista para cada década
top5_artistas_por_decada <- dados_portugues %>%
  group_by(Decada, Artista.x) %>%  # Agrupar por década e artista
  count() %>%  # Contar a quantidade de músicas por artista dentro de cada década
  arrange(Decada, desc(n)) %>%  # Ordenar por década e pela quantidade de músicas
  group_by(Decada) %>%
  slice_head(n = 5) %>%  # Selecionar os top 5 artistas de cada década
  ungroup() %>%
  select(Decada, Artista.x, n)  # Selecionar as colunas necessárias

# Exibir os top 5 artistas por década
print(top5_artistas_por_decada)
View(top5_artistas_por_decada)
#######################################################################
dados_portugues <- data_final %>%
  filter(idiomas == "pt")

# Criar a coluna 'Decada' com base no ano
dados_portugues <- dados_portugues %>%
  mutate(Decada = paste0(substr(Ano, 1, 3), "0"))

# Contar músicas por artista por década
top5_artistas_por_decada <- dados_portugues %>%
  group_by(Decada, Artista.x) %>%
  summarise(Frequencia = n(), .groups = "drop") %>%
  arrange(Decada, desc(Frequencia)) %>%
  group_by(Decada) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  mutate(Posicao = row_number()) %>%
  select(Decada, Artista.x, Posicao)

# Agora, vamos alinhar os artistas por década, sem gerar NA
top5_artistas_por_decada_matriz <- top5_artistas_por_decada %>%
  pivot_wider(names_from = Decada, values_from = Artista.x, values_fn = list) %>%
  mutate(across(everything(), ~sapply(.x, function(x) if(length(x) == 0) NA else x)))

library(xtable)

xtable(top5_artistas_por_decada_matriz[,-1])
View(top5_artistas_por_decada_matriz)

#########################################################

linhas_com_na <- which(rowSums(is.na(data_final)) > 0)

if (length(linhas_com_na) > 0) {
  print(paste("Linhas com NA:", paste(linhas_com_na, collapse = ", ")))
} else {
  print("Não há linhas com NA no dataframe.")
}

View(data_final[c(139, 201, 1079, 1187, 1225, 1786, 2363, 2427, 2579, 3379),])
#########################################################################

# Lista personalizada de stopwords
custom_stopwords <- c("pra", "assim", "mim", "vou", "ela", "ele", "de", "a", "o", "e", "um", "uma")


# Criação do corpus_decada
corpus_decada <- data_final_pt %>%
  mutate(Decada = as.character(floor(Ano / 10) * 10)) %>% # Cria década como texto
  group_by(Decada) %>%
  summarise(texto = paste(Letra, collapse = " ")) %>%
  ungroup()

# Lista para armazenar palavras por década
palavras_por_decada <- list()

# Loop por década
for(i in unique(corpus_decada$Decada)) {
  # Filtra músicas da década
  musica_decada <- corpus_decada %>%
    filter(Decada == i)
  
  # Cria o corpus
  corpus <- VCorpus(VectorSource(musica_decada$texto))
  
  # Limpeza do texto com stopwords personalizadas
  corpus_clean <- corpus %>%
    tm_map(content_transformer(tolower)) %>%      # Converte para minúsculas
    tm_map(removePunctuation) %>%                 # Remove pontuação
    tm_map(removeNumbers) %>%                    # Remove números
    tm_map(removeWords, stopwords("pt")) %>%      # Remove stopwords padrão
    tm_map(removeWords, custom_stopwords) %>%     # Remove stopwords personalizadas
    tm_map(stripWhitespace)                      # Remove espaços extras
  
  # Matriz de termos
  dtm <- DocumentTermMatrix(corpus_clean, control = list(stopwords = TRUE))
  
  # Frequência das palavras
  freq <- colSums(as.matrix(dtm))
  freq_df <- data.frame(Palavra = names(freq), Frequencia = freq, row.names = NULL)
  
  # Seleciona as 100 palavras mais frequentes
  top_100 <- freq_df %>%
    arrange(desc(Frequencia)) %>%
    slice_head(n = 100)
  
  # Armazena só as palavras
  palavras_por_decada[[i]] <- top_100$Palavra
}

# Junta as palavras em colunas por década, ajustando o tamanho para 100 linhas
max_length <- max(sapply(palavras_por_decada, length))
top_palavras_decada1 <- do.call(cbind, lapply(palavras_por_decada, function(x) {
  length(x) <- max_length
  return(x)
}))

# Ajusta os nomes das colunas (décadas)
colnames(top_palavras_decada1) <- names(palavras_por_decada)

# Converte para data.frame
top_palavras_decada1 <- as.data.frame(top_palavras_decada1)

# Visualiza o resultado
View(top_palavras_decada1)
#########################################################################
install.packages("stopwords")
library(stopwords)

# Exemplo de texto e lista de stopwords
texto <- c("Esse é um exemplo de texto com stopwords como pra, assim, mim.")
stopwords_pt <- stopwords::stopwords("pt")  # Lista de stopwords em português

# Remover stopwords
texto_sem_stopwords <- gsub(paste(stopwords_pt, collapse = "|"), "", texto)

#############################################################################

library(tm)

# Exemplo de corpus com um texto simples
texto <- c("Este é um exemplo pra  de texto com várias stopwords. Como por exemplo, 'de', 'um', 'e'.")

# Criar o corpus
corpus <- VCorpus(VectorSource(texto))

# Remover stopwords usando tm_map
corpus_clean <- tm_map(corpus, removeWords, stopwords("pt"))

# Visualizar o texto limpo
inspect(corpus_clean)

clean_text <- sapply(corpus_clean, as.character)
print(clean_text)


stopwords_pt <- stopwords("pt")
print(stopwords_pt)


##################################################################################

# Criar sua própria lista de stopwords, removendo palavras que você não quer
stopwords_personalizado <- setdiff(stopwords("pt"), c("dados"))

# Remove stopwords usando sua lista personalizada
corpus_clean <- tm_map(corpus, removeWords, stopwords_personalizado)

# Ver resultado
clean_text <- content(corpus_clean[[1]])
print(clean_text)
##################################################################################

linhas_com_palavra <- grep("calipso", data_final_pt$Letra[1:200], ignore.case = TRUE)
musicas_com_palavra <- data_final_pt$Nome[linhas_com_palavra]
print(musicas_com_palavra)
print(data_final_pt$ID[linhas_com_palavra])

