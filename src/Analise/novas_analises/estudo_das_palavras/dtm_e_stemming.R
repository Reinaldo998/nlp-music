# Este bloco cria as DTMs stemizadas (menos precisas linguisticamente).
library(tm)
library(stopwords)
library(SnowballC) # Para Stemming
library(dplyr)

# --- 1. Pré-processamento Básico com o Pacote 'tm' ---
corpus_letras_pt <- VCorpus(VectorSource(data_final_pt$Letra))

corpus_letras_pt <- tm_map(corpus_letras_pt, content_transformer(tolower))
corpus_letras_pt <- tm_map(corpus_letras_pt, removePunctuation)
corpus_letras_pt <- tm_map(corpus_letras_pt, removeNumbers)
stopwords_pt <- stopwords("pt")
corpus_letras_pt <- tm_map(corpus_letras_pt, removeWords, stopwords_pt)
corpus_letras_pt <- tm_map(corpus_letras_pt, stripWhitespace)


# --- 2. STEMMING: Substituindo a Lematização ---
corpus_letras_stemizadas <- tm_map(corpus_letras_pt, stemDocument, language = "portuguese")


# --- 3. Criação da DTM E ATRIBUIÇÃO MANUAL DOS NOMES DE LINHA ---
# Criamos o DTM com os termos stemizados
dtm_letras_stem <- DocumentTermMatrix(corpus_letras_stemizadas)

# A ordem é garantida pois o tm_map preserva a ordem original do data_final_pt.
row.names(dtm_letras_stem) <- as.character(data_final_pt$ID)

matrix_dtm_stem <- as.matrix(dtm_letras_stem)


# --- 4. Análise de Frequência e Filtragem da DTM ---
frequencia_total_palavras_stem <- colSums(matrix_dtm_stem) # Vetor de frequências stemizadas

min_frequencia <- 30
max_frequencia <- 1000

palavras_selecionadas_stem <- names(frequencia_total_palavras_stem[
  frequencia_total_palavras_stem >= min_frequencia &
    frequencia_total_palavras_stem <= max_frequencia
])

# Cria a DTM FILTRADA STEMIZADA
dtm_filtrada_stem <- dtm_letras_stem[, colnames(dtm_letras_stem) %in% palavras_selecionadas_stem]

#==========================================================================================
dtm_total_bruto <- DocumentTermMatrix(corpus_letras_pt)
contagem_total_bruto <- ncol(dtm_total_bruto)

contagem_lemmatizada <- ncol(dtm_letras_tf)  # DTM criada pelo udpipe
contagem_stemizada <- ncol(dtm_letras_stem) # DTM criada pelo SnowballC
#=======================================================================================

palavra_stemizada_alvo <- "dua"

# Verifica se o radical existe como uma coluna (termo) na DTM
if (!(palavra_stemizada_alvo %in% colnames(dtm_letras_stem))) {
  stop(paste0("ERRO: O termo stemizado '", palavra_stemizada_alvo, "' não foi encontrado na DTM."))
}

# Obtém o vetor de frequência do stem na DTM
frequencias_do_stem <- as.vector(dtm_letras_stem[, palavra_stemizada_alvo])

# Identifica os Códigos das músicas onde a frequência é maior que 0
codigos_com_stem <- as.numeric(row.names(dtm_letras_stem)[which(frequencias_do_stem > 0)])


# --- 3. Juntar com os Metadados Originais ---

if (length(codigos_com_stem) == 0) {
  cat("AVISO: Nenhum documento contém este radical.\n")
} else {
  # Filtra o dataframe principal para pegar as músicas correspondentes
  df_musicas_encontradas <- data_final_pt %>%
    filter(ID %in% codigos_com_stem) %>%
    select(
      ID_Musica = ID,
      Nome_Musica = Nome.x,
      Artista = Artista.x,
      Letra_Original = Letra # Letra da base original (não processada)
    )
  
  # --- 4. Exibir o Resultado (Metadados e Diagnóstico) ---
  
  cat(paste0("Total de músicas encontradas com o radical: ", nrow(df_musicas_encontradas), "\n"))
  cat("------------------------------------------------------------------------\n")
  
  # Para visualização das músicas (metadados)
  print("Metadados das músicas que contêm o radical:")
  View(df_musicas_encontradas)
  print(head(df_musicas_encontradas))
  
  # Para verificar as palavras originais (necessita de análise separada no corpus original)
  cat("\nAVISO: As palavras originais exatas (tokens) precisam ser verificadas manualmente no texto da Letra_Original.\n")
}



