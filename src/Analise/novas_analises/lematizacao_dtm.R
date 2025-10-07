library(tm)        # Para corpus e pré-processamento de texto
library(stopwords) # Para lista de stopwords em português
library(udpipe)    # Para lematização
library(dplyr)     # Para manipulação de dataframes (pipelines, summarise, filter)
library(compiler)

remove_acentos_e_especiais <- function(texto) {
  if (is.character(texto)) {
    # 1. Remove acentos
    texto <- chartr(
      "áéíóúÁÉÍÓÚàèìòùüÀÈÌÒÙãõñÃÕÑâêîôûÂÊÎÔÛÜçÇ",
      "aeiouAEIOUaeiouuAEIOUaonAONaeiouAEIOUUcC",
      texto
    )
    # 2. Remove todos os caracteres que não são letras ou espaços (proteção extra)
    texto <- gsub("[^[:alnum:][:space:]]", "", texto)
  }
  return(texto)
}

# --- Carregar o Modelo udpipe ---
# Use o caminho e o nome do arquivo EXATOS que o download informou quando você baixou o modelo.
# ATUALIZE ESTE CAMINHO PARA O SEU ARQUIVO DO MODELO UDPIPE
ud_model <- udpipe_load_model(file = "/home/reinaldo/portuguese-bosque-ud-2.5-191206.udpipe")

# --- 1. Pré-processamento Básico com o Pacote 'tm' ---
corpus_letras_pt <- VCorpus(VectorSource(data_final_pt$Letra))

corpus_letras_pt <- tm_map(corpus_letras_pt, content_transformer(tolower))

corpus_letras_pt <- tm_map(corpus_letras_pt, content_transformer(
  function(x) gsub("[[:digit:]]+", " ", x)
))

corpus_letras_pt <- tm_map(corpus_letras_pt, content_transformer(
  function(x) gsub("[[:punct:]]", " ", x)
))

corpus_letras_pt <- tm_map(corpus_letras_pt, content_transformer(remove_acentos_e_especiais))

corpus_letras_pt <- tm_map(corpus_letras_pt, stripWhitespace)

stopwords_pt <- stopwords("pt")
corpus_letras_pt <- tm_map(corpus_letras_pt, removeWords, stopwords_pt)


letras_df <- data.frame(doc_id = data_final_pt$ID, # Usa o Codigo real como ID do documento
                        text = sapply(corpus_letras_pt, as.character),
                        stringsAsFactors = FALSE)
#letras_df[22,2]


enableJIT(3)
#letras_anotadas <- udpipe_annotate(ud_model, x = letras_df$text, doc_id = letras_df$doc_id)
letras_anotadas_df <- as.data.frame(letras_anotadas)

#############################################################################
letras_lematizadas_por_musica <- letras_anotadas_df %>%
  filter(!is.na(lemma)) %>%
  group_by(doc_id) %>%
  summarise(lemmas_texto = paste(lemma, collapse = " "))

corpus_letras_lematizadas <- VCorpus(VectorSource(letras_lematizadas_por_musica$lemmas_texto))
dtm_letras_tf <- DocumentTermMatrix(corpus_letras_lematizadas)
row.names(dtm_letras_tf) <- as.character(letras_lematizadas_por_musica$doc_id)

matrix_dtm <- as.matrix(dtm_letras_tf)
