library(dplyr)
library(tm) # Para funções de pré-processamento
library(SnowballC) # Para stopwords (se usado no pré-processamento)
library(udpipe) # Para o modelo ud_model e letras_anotadas_df

# Certifique-se de que os seguintes objetos estão disponíveis em sua sessão R:
# - df_musicas_portugues (seu dataframe original de músicas em português)
# - letras_anotadas_df (o dataframe detalhado com token, lemma, doc_id etc., gerado pelo udpipe)
# - ud_model (o modelo udpipe carregado)
# - stopwords("portuguese") (as stopwords usadas)


# --- 1. Defina a PALAVRA ORIGINAL (não lematizada) que você quer investigar ---
# ATENÇÃO: SUBSTITUA PELA SUA PALAVRA REAL (Ex: "cantando", "coração", "você")
palavra_original_para_investigar <- "afir" 

print(paste0("Investigando a palavra original: '", palavra_original_para_investigar, "'"))


# --- 2. Processar a palavra original com o mesmo pipeline de lematização do UDPIPE ---
# Isso garante que a palavra de busca seja tratada da mesma forma que as palavras na DTM.

# 2.1. Pré-processamento básico do TM para a palavra de busca
temp_corpus_investigar <- VCorpus(VectorSource(palavra_original_para_investigar))
temp_corpus_investigar <- tm_map(temp_corpus_investigar, content_transformer(tolower))
temp_corpus_investigar <- tm_map(temp_corpus_investigar, removePunctuation)
temp_corpus_investigar <- tm_map(temp_corpus_investigar, removeNumbers)
# A remoção de stopwords aqui pode ser opcional, dependendo de como você quer que a palavra seja tratada.
# Se a palavra original for uma stopword, ela será removida e o lemma será NA.
temp_corpus_investigar <- tm_map(temp_corpus_investigar, removeWords, stopwords("portuguese")) 
temp_corpus_investigar <- tm_map(temp_corpus_investigar, stripWhitespace)
cleaned_investigate_text <- as.character(temp_corpus_investigar[[1]])

# 2.2. Lematizar com udpipe (usando o 'ud_model' carregado)
temp_df_investigate <- data.frame(doc_id = "investigate_query", text = cleaned_investigate_text, stringsAsFactors = FALSE)
temp_annotated_investigate <- udpipe_annotate(ud_model, x = temp_df_investigate$text, doc_id = temp_df_investigate$doc_id)
temp_annotated_investigate_df <- as.data.frame(temp_annotated_investigate)

# 2.3. Obter o lemma principal da palavra input
lemma_da_palavra_input <- temp_annotated_investigate_df$lemma[1]

# --- Verificações de segurança para o lemma ---
if (is.na(lemma_da_palavra_input) || nchar(lemma_da_palavra_input) == 0 ||
    lemma_da_palavra_input %in% stopwords("portuguese")) { # Se a palavra original virou stopword ou vazio
  print(paste0("AVISO: A palavra original '", palavra_original_para_investigar, "' resultou em um lemma vazio, NA, ou é uma stop word após o processamento. Verifique a palavra."))
  # Você pode querer inspecionar temp_annotated_investigate_df para entender por que.
  View(temp_annotated_investigate_df)
} else {
  print(paste0("A palavra original '", palavra_original_para_investigar, "' foi lematizada para: '", lemma_da_palavra_input, "'"))
  
  # --- 3. Encontrar todas as ocorrências deste lemma e seus tokens originais em 'letras_anotadas_df' ---
  ocorrencias_do_lemma <- letras_anotadas_df %>%
    filter(lemma == lemma_da_palavra_input) %>%
    # Seleciona colunas relevantes para o detalhe e remove duplicatas de (doc_id, token, lemma)
    select(doc_id, sentence_id, token, lemma, upos, xpos) %>% 
    distinct(doc_id, token, lemma, .keep_all = TRUE) # Pega ocorrências únicas de (documento, forma_original_da_palavra, lemma)
  
  if (nrow(ocorrencias_do_lemma) > 0) {
    # 4. Encontrar as músicas originais correspondentes (usando os doc_ids que são os Códigos)
    codigos_das_musicas_com_lemma <- unique(ocorrencias_do_lemma$doc_id)
    musicas_onde_o_lemma_aparece <- df_musicas_portugues %>%
      filter(Codigo %in% codigos_das_musicas_com_lemma)
    
    print(paste0("O lemma '", lemma_da_palavra_input, "' (da palavra original '", palavra_original_para_investigar, "') aparece em ", nrow(musicas_onde_o_lemma_aparece), " músicas."))
    
    print("--- MÚSICAS ONDE O LEMMA FOI ENCONTRADO ---")
    View(musicas_onde_o_lemma_aparece) # Veja as músicas originais
    print(head(musicas_onde_o_lemma_aparece))
    
    print("--- DETALHES DA LEMATIZAÇÃO E TOKENS ORIGINAIS ---")
    print(paste0("A palavra original '", palavra_original_para_investigar, "' foi lematizada para '", lemma_da_palavra_input, "'."))
    print("Ela apareceu nas músicas através das seguintes formas originais (tokens):")
    View(ocorrencias_do_lemma %>% select(doc_id, token, lemma, upos)) # Veja as palavras originais (tokens) e seus lemmas
    print(head(ocorrencias_do_lemma %>% select(doc_id, token, lemma, upos)))
    
  } else {
    print(paste0("O lemma '", lemma_da_palavra_input, "' (da palavra original '", palavra_original_para_investigar, "') não foi encontrado em nenhuma música."))
  }
}