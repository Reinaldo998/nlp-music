# Instalar pacotes (se ainda não tiverem sido instalados)
#install.packages("tm")
#install.packages("stopwords")
#install.packages("udpipe")
#install.packages("dplyr") # Para manipulação de dados, útil para a preparação

# Carregar pacotes
library(tm)
library(stopwords)
library(udpipe)
library(dplyr)

# Criar um objeto Corpus a partir da coluna 'letra_da_musica'
#corpus_letras_pt <- VCorpus(VectorSource(data_final_pt$Letra))
corpus_letras_pt <- VCorpus(VectorSource(df_musicas_portugues$Letra_Site))
# Converter para minúsculas
corpus_letras_pt <- tm_map(corpus_letras_pt, content_transformer(tolower))

# Remover pontuação
corpus_letras_pt <- tm_map(corpus_letras_pt, removePunctuation)

# Remover números
corpus_letras_pt <- tm_map(corpus_letras_pt, removeNumbers)

# Remover stopwords em português
# Você pode obter a lista de stopwords para português usando o pacote 'stopwords'
stopwords_pt <- stopwords("pt")
corpus_letras_pt <- tm_map(corpus_letras_pt, removeWords, stopwords_pt)

# Opcional: Remover espaços em branco extras que podem ter sido criados
corpus_letras_pt <- tm_map(corpus_letras_pt, stripWhitespace)

# Fazer o download do modelo para português (execute apenas uma vez)
#download_model("portuguese-bosque", "portuguese-bosque.udpipe")
udpipe_download_model(language = "portuguese-bosque")
# Ou você pode usar 'portuguese' se não houver um modelo específico como 'portuguese-bosque'.
# Por exemplo: ud_model <- udpipe_download_model(language = "portuguese")
# Verifique os modelos disponíveis com udpipe_get_available_models()

# Use o caminho e o nome do arquivo EXATOS que o download informou
ud_model <- udpipe_load_model(file = "/home/reinaldo/portuguese-bosque-ud-2.5-191206.udpipe")

# Criar um data.frame a partir do corpus para processar com udpipe
letras_df <- data.frame(doc_id = seq_along(df_musicas_portugues$Letra_Site),
                        text = sapply(corpus_letras_pt, as.character),
                        stringsAsFactors = FALSE)
View(letras_df)

# Anotar o texto (tokenização, POS tagging, lematização)
# Isso pode levar um tempo dependendo do tamanho do seu corpus
letras_anotadas <- udpipe_annotate(ud_model, x = letras_df$text, doc_id = letras_df$doc_id)
letras_anotadas_df <- as.data.frame(letras_anotadas)

# Para a Bag-of-Words, vamos usar as 'lemma's.
# Agrupar as lemmas por documento (música)
letras_lematizadas_por_musica <- letras_anotadas_df %>%
  filter(!is.na(lemma)) %>% # Remover NAs, que podem ser de pontuações ou números
  group_by(doc_id) %>%
  summarise(lemmas_texto = paste(lemma, collapse = " "))

# Atualizar o corpus com as letras lematizadas
# É importante garantir que a ordem dos documentos seja mantida
corpus_letras_lematizadas <- VCorpus(VectorSource(letras_lematizadas_por_musica$lemmas_texto[order(as.numeric(letras_lematizadas_por_musica$doc_id))]))

# Criar a Document-Term Matrix (DTM) com Term Frequency (TF)
# Por padrão, o TermDocumentMatrix já calcula as frequências (TF)
# Usaremos o corpus_letras_lematizadas que você gerou
dtm_letras_tf <- DocumentTermMatrix(corpus_letras_lematizadas)

# Você pode inspecionar a DTM (as primeiras 5 linhas e 10 colunas, por exemplo)
# Isso ajuda a entender a estrutura
inspect(dtm_letras_tf[1:5, 1:10])

# Se preferir ver como uma matriz normal (pode ser grande para muitos termos/documentos)
#as.matrix(dtm_letras_tf)


# Converter a DTM em uma matriz para facilitar a soma das colunas
matrix_dtm <- as.matrix(dtm_letras_tf)

# Calcular a soma de cada coluna (cada termo)
frequencia_total_palavras <- colSums(matrix_dtm)

# Ordenar as palavras pela frequência em ordem decrescente
frequencia_total_palavras_ordenada <- sort(frequencia_total_palavras, decreasing = TRUE)

# Visualizar as 20 palavras mais frequentes
head(frequencia_total_palavras_ordenada, 20)
hist(frequencia_total_palavras_ordenada)
dim(matrix_dtm)
#==============================================================================

# A lista 'frequencia_total_palavras_ordenada' já está em ordem decrescente de frequência.
# Para ver as palavras pouco frequentes, podemos olhar o final dessa lista.

# 1. Palavras que aparecem apenas uma vez (as mais raras)
# Estas são as últimas palavras na sua lista ordenada
cat("--- Palavras que aparecem apenas 1 vez ---\n")
palavras_frequencia_1 <- frequencia_total_palavras_ordenada[frequencia_total_palavras_ordenada == 1]
print(head(palavras_frequencia_1, 20)) # Mostra as primeiras 20 palavras que aparecem só uma vez
cat("Total de palavras que aparecem apenas 1 vez:", length(palavras_frequencia_1), "\n\n")

# 2. Palavras pouco frequentes (ex: que aparecem até 5 vezes)
# Vamos pegar as palavras que aparecem 5 vezes ou menos.
frequencia_maxima_pouco_frequente <- 5 # Defina seu limite para "pouco frequente"

cat(paste0("--- Palavras que aparecem até ", frequencia_maxima_pouco_frequente, " vezes ---\n"))
palavras_pouco_frequentes <- frequencia_total_palavras_ordenada[frequencia_total_palavras_ordenada <= frequencia_maxima_pouco_frequente]
length(palavras_pouco_frequentes)
# Imprimir as 20 primeiras palavras dessa categoria
print(head(palavras_pouco_frequentes, 20))
cat(paste0("Total de palavras que aparecem até ", frequencia_maxima_pouco_frequente, " vezes: ", length(palavras_pouco_frequentes), "\n\n"))

length(palavras_pouco_frequentes)/ncol(matrix_dtm)
# Para inspecionar TODAS as palavras pouco frequentes (cuidado com o volume se for muito grande):
# View(palavras_pouco_frequentes)
#================================================================================================================

#================================================================================================================

# Definir os limites de frequência
min_frequencia <- 20
max_frequencia <- 205000

# Filtrar as palavras (lemmas) com base nas frequências desejadas
# 'frequencia_total_palavras' é o vetor nomeado que contém a frequência de cada palavra
palavras_selecionadas <- names(frequencia_total_palavras[
  frequencia_total_palavras > min_frequencia &
    frequencia_total_palavras < max_frequencia
])

length(palavras_selecionadas)
# Criar a nova DTM (Document-Term Matrix) contendo apenas as palavras selecionadas
# Usamos a dtm_letras_tf original e selecionamos apenas as colunas correspondentes às palavras_selecionadas
dtm_filtrada <- dtm_letras_tf[, colnames(dtm_letras_tf) %in% palavras_selecionadas]

min(colSums(as.matrix(dtm_filtrada)))
# --- Verificações (Opcional) ---
cat("Dimensões da DTM original:\n")
print(dim(dtm_letras_tf))

cat("\nDimensões da nova DTM filtrada:\n")
print(dim(dtm_filtrada))

# Inspecionar as primeiras linhas e colunas da DTM filtrada
# Ajuste os números se a DTM filtrada for muito pequena
if (nrow(dtm_filtrada) > 0 && ncol(dtm_filtrada) > 0) {
  cat("\nPrimeiras 5 linhas e 10 colunas da DTM filtrada (se houver):\n")
  print(as.matrix(dtm_filtrada[1:min(5, nrow(dtm_filtrada)), 1:min(10, ncol(dtm_filtrada))]))
} else {
  cat("\nA DTM filtrada está vazia ou muito pequena para exibir as primeiras linhas/colunas.\n")
}

colSums(as.matrix(dtm_filtrada[,-c(1,2)]))
dtm_filtrada<-dtm_filtrada[,-c(1,2)]
# Você também pode verificar algumas das palavras que estão na nova DTM
cat("\nAlgumas das palavras na DTM filtrada:\n")
print(head(colnames(dtm_filtrada), 20))

# Salvar a dtm_filtrada em um arquivo RDS
saveRDS(dtm_filtrada, file = "dtm_filtrada_30.rds")
