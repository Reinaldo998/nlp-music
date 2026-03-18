# Este bloco de código é autônomo para a criação da DTM e suas transformações.
#
# OBJETIVO: Pegar letras de músicas e gerar DTMs lematizadas e filtradas por frequência.
#
# ENTRADA NECESSÁRIA:
# 1. df_musicas_portugues: Um dataframe com as músicas em português,
#                           contendo OBRIGATORIAMENTE as colunas 'Letra_Site' (texto das letras)
#                           e 'Codigo' (o ID único de cada música).
#
# ATENÇÃO: Se 'df_musicas_portugues' não estiver disponível em sua sessão,
#          DESCOMENTE e ATUALIZE a linha de carregamento abaixo.
# df_musicas_portugues <- readRDS("caminho/para/seu/df_musicas_portugues.rds")


# --- Carregar os pacotes necessários ---
library(tm)        # Para corpus e pré-processamento de texto
library(stopwords) # Para lista de stopwords em português
library(udpipe)    # Para lematização
library(dplyr)     # Para manipulação de dataframes (pipelines, summarise, filter)


# --- Carregar o Modelo udpipe ---
# Use o caminho e o nome do arquivo EXATOS que o download informou quando você baixou o modelo.
# ATUALIZE ESTE CAMINHO PARA O SEU ARQUIVO DO MODELO UDPIPE
ud_model <- udpipe_load_model(file = "/home/reinaldo/portuguese-bosque-ud-2.5-191206.udpipe")
View(data_final_pt)
dim(corpus_letras_pt)
# --- 1. Pré-processamento Básico com o Pacote 'tm' ---
#corpus_letras_pt <- VCorpus(VectorSource(df_musicas_portugues$Letra_Site))
corpus_letras_pt <- VCorpus(VectorSource(data_final_pt$Letra))

corpus_letras_pt <- tm_map(corpus_letras_pt, content_transformer(tolower))
corpus_letras_pt <- tm_map(corpus_letras_pt, removePunctuation)
corpus_letras_pt <- tm_map(corpus_letras_pt, removeNumbers)
stopwords_pt <- stopwords("pt")
corpus_letras_pt <- tm_map(corpus_letras_pt, removeWords, stopwords_pt)
corpus_letras_pt <- tm_map(corpus_letras_pt, stripWhitespace)

View(corpus_letras_pt)
# --- 2. Lematização com 'udpipe' ---
letras_df <- data.frame(doc_id = data_final_pt$ID, # Usa o Codigo real como ID do documento
                        text = sapply(corpus_letras_pt, as.character),
                        stringsAsFactors = FALSE)
library(compiler)
enableJIT(3)
letras_anotadas <- udpipe_annotate(ud_model, x = letras_df$text, doc_id = letras_df$doc_id)
letras_anotadas_df <- as.data.frame(letras_anotadas)


letras_lematizadas_por_musica <- letras_anotadas_df %>%
  filter(!is.na(lemma)) %>%
  group_by(doc_id) %>%
  summarise(lemmas_texto = paste(lemma, collapse = " "))

# --- 3. Criação da DTM E A ATRIBUIÇÃO MANUAL DOS NOMES DE LINHA ---
# 3.1: Cria o corpus lematizado
corpus_letras_lematizadas <- VCorpus(VectorSource(letras_lematizadas_por_musica$lemmas_texto))
#corpus_letras_lematizadas <- VCorpus(VectorSource(letras_lematizadas_por_musica$lemmas_texto[order(as.numeric(letras_lematizadas_por_musica$doc_id))]))

# 3.2: Cria a DTM (que terá nomes de linha sequenciais)
dtm_letras_tf <- DocumentTermMatrix(corpus_letras_lematizadas)
dim(dtm_letras_tf)
# 3.3: CORREÇÃO CRÍTICA: Atribui os Códigos reais da música como nomes de linha
# A ordem das linhas de 'letras_lematizadas_por_musica' é a mesma dos documentos na DTM.
row.names(dtm_letras_tf) <- as.character(letras_lematizadas_por_musica$doc_id)

matrix_dtm <- as.matrix(dtm_letras_tf)

row.names(dtm_letras_tf)

# --- 4. Análise de Frequência e Filtragem da DTM ---
frequencia_total_palavras <- colSums(matrix_dtm)

# Calcula a frequência total de cada palavra (somando as colunas da DTM)
#frequencia_total_palavras <- colSums(as.matrix(dtm_filtrada))

# Ordena o vetor de frequências em ordem decrescente
palavras_ordenadas_por_frequencia <- sort(frequencia_total_palavras, decreasing = F)

# Imprime a lista completa no console
print(palavras_ordenadas_por_frequencia[8768:11000])

min_frequencia <- 30
max_frequencia <- 1000

palavras_selecionadas <- names(frequencia_total_palavras[
  frequencia_total_palavras >= min_frequencia &
    frequencia_total_palavras <= max_frequencia
])

dtm_filtrada <- dtm_letras_tf[, colnames(dtm_letras_tf) %in% palavras_selecionadas]
dim(dtm_filtrada)
row.names(dtm_filtrada)
print("Etapa 4: DTM filtrada por frequência (dtm_filtrada) criada.")
print(paste0("Dimensões: ", paste(dim(dtm_filtrada), collapse = "x")))
print("Nomes das 20 primeiras linhas (Códigos) na DTM filtrada:")
print(head(row.names(dtm_filtrada), 20))

max(colSums(as.matrix(dtm_filtrada)))
min(colSums(as.matrix(dtm_filtrada)))
# --- 5. Criação da DTM TF-IDF ---
dtm_letras_tfidf <- weightTfIdf(dtm_filtrada)


# --- OBJETOS DE SAÍDA DESTE BLOCO ---
# - dtm_filtrada (DTM de frequência de termos filtrada, com nomes de linha corretos)
# - dtm_letras_tfidf (DTM TF-IDF filtrada, com nomes de linha corretos)
# - letras_anotadas_df (dataframe com anotações de udpipe)


print("Etapa 5: Matriz Documento-Termo (DTM) TF-IDF criada: dtm_letras_tfidf.")
print(paste0("Dimensões: ", paste(dim(dtm_letras_tfidf), collapse = "x")))
print("Termos mais importantes na DTM TF-IDF (Top 20):")
print(head(sort(colSums(as.matrix(dtm_letras_tfidf)), decreasing = TRUE), 20))

dtm_matrix <- as.matrix(dtm_filtrada)
total_palavras_por_musica <- rowSums(dtm_matrix)
dtm_tf_normalizada <- dtm_matrix / total_palavras_por_musica

min(total_palavras_por_musica)
#mteste<-matrix(c(8,2,30,70),2,2,T)
#soml<-rowSums(mteste)
#mteste/soml
# --- OBJETOS DE SAÍDA DESTE BLOCO ---
# - dtm_filtrada (DTM de frequência de termos filtrada)
# - dtm_letras_tfidf (DTM TF-IDF filtrada)
# - letras_anotadas_df (dataframe com todas as anotações de udpipe, útil para depuração ou análises detalhadas)


# --- Opcional: Salvar DTMs ---
#saveRDS(dtm_filtrada, file = "dtm_filtrada_tf_30.rds")
#saveRDS(dtm_letras_tfidf, file = "dtm_letras_tfidf_30.rds")
#saveRDS(letras_anotadas_df, file = "letras_anotadas_df.rds")


# Assumindo que dtm_letras_tf e letras_lematizadas_por_musica estão na sua sessão
#row.names(dtm_letras_tf) <- as.character(letras_lematizadas_por_musica$doc_id)
