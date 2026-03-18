#install.packages('cld2')
#install.packages('textcat')
library(cld2)
library(textcat)
library(franc)

corps<-df_final_para_analise_total_unique$Letra_Site %>% VectorSource()%>%VCorpus()

textos <- sapply(corps, as.character)

########### Idioma 1 ###########
idiomas <- cld2::detect_language(textos)
length(idiomas)

########### Idioma 2 ###########
idio <- textcat(textos)

length(idio)
########### Idioma 3 ###########
devtools::install_github("marcellodesales/cld3")
idiomas <- detect_language(textos)


Mqtdade<-cbind(df_final_para_analise_total_unique,idiomas)

View(Mqtdade)

# --- Filtrar apenas as músicas em português ---
df_musicas_portugues <- Mqtdade %>%
  filter(idiomas == "pt")

#df_musicas_portugues <- Mqtdade %>%
#  filter(idio == "portuguese")

# --- Visualizar o resultado ---
print("Músicas filtradas apenas em Português:")
View(df_musicas_portugues)
print(dim(df_musicas_portugues))


# Opcional: Salvar o dataframe
# write.csv(df_musicas_portugues, "df_musicas_portugues.csv", row.names = FALSE, fileEncoding = "UTF-8")

num_nas_idiomas <- sum(is.na(Mqtdade$idiomas))
print(paste0("Número de músicas com idioma NÃO detectado (NA): ", num_nas_idiomas))

print("Músicas com Idioma NÃO Detectado (NA):")
View(Mqtdade %>%
       filter(is.na(idiomas)))

print("Visualizando músicas que NÃO estão em Português:")
View(Mqtdade %>%
       filter(idiomas != "pt"))

View(Mqtdade %>%
       filter(idio != "pt"))

dim(Mqtdade)

#==========================================================================================================
codigos_para_corrigir_para_pt <- c(6530, 7569, 7690,7910,8332) # Exemplo: Códigos que devem ser 'pt'

print(paste0("Número de Códigos a serem corrigidos para 'pt': ", length(codigos_para_corrigir_para_pt)))

# --- 2. Obtenha as músicas a serem corrigidas do Mqtdade original ---
musicas_a_corrigir_idioma <- Mqtdade %>%
  filter(Codigo %in% codigos_para_corrigir_para_pt) %>%
  mutate(idiomas = "pt") # Força o idioma para "pt" para essas músicas

print("Músicas a serem corrigidas e adicionadas ao df_musicas_portugues:")
View(musicas_a_corrigir_idioma)
print(dim(musicas_a_corrigir_idioma))

# --- 3. Combine o df_musicas_portugues existente com as músicas corrigidas ---
# Usamos bind_rows para empilhar os dataframes.
# Usamos distinct(Codigo, .keep_all = TRUE) para garantir que cada Codigo seja único.
# Como 'musicas_a_corrigir_idioma' vem depois, se houver um Codigo em comum,
# a versão de 'musicas_a_corrigir_idioma' (com o idioma "pt" forçado) será mantida.
df_musicas_portugues_atualizado <- bind_rows(
  df_musicas_portugues,          # O dataframe de músicas em português atual
  musicas_a_corrigir_idioma      # As músicas que acabamos de corrigir o idioma para "pt"
) %>%
  distinct(Codigo, .keep_all = TRUE) # Remove duplicatas de Codigo, mantendo a última ocorrência (a corrigida)

# --- 4. Atualize o dataframe original df_musicas_portugues (se desejar) ---
# Se você quiser que a variável 'df_musicas_portugues' reflita essa atualização,
# sobrescreva-a com o novo dataframe.
df_musicas_portugues <- df_musicas_portugues_atualizado

# --- 5. Verifique o novo dataframe de músicas em português ---
print("Novo Dataframe de Músicas em Português (após correção e reinclusão):")
View(df_musicas_portugues)
print(paste0("Novo total de músicas em Português: ", nrow(df_musicas_portugues)))

# Opcional: Salvar o dataframe
#write.csv(df_musicas_portugues, "df_musicas_portugues.csv", row.names = FALSE, fileEncoding = "UTF-8")
#======================================================================================================

#==========================================================================================================================================================
# Reordenando data_final_pt
View(data_final_pt_1)
#####data_final_pt_1<-data_final_pt
colnames(data_final_pt_1)
colnames(df_musicas_portugues)
data_final_pt <- df_musicas_portugues
colnames(data_final_pt) <- c("ID", "Nome.x", "Artista.x", "Ano", "Nome.y", "Artista.y", "Letra", "Idioma")

# Verifique o dataframe ANTES da reordenação
print("Dataframe ANTES da reordenação:")
print(head(data_final_pt))
print(colnames(data_final_pt))

# --- Reordenar as colunas ---

# A coluna "ID" é a primeira. Queremos que ela vá para a QUARTA posição.
# Isso significa que as colunas que eram a 2ª, 3ª e 4ª agora serão as 1ª, 2ª e 3ª, respectivamente.

data_final_pt_reordenado <- data_final_pt %>%
  select(
    Nome.x,      # Coluna que era a 2ª, agora é a 1ª
    Artista.x,   # Coluna que era a 3ª, agora é a 2ª
    Ano,         # Coluna que era a 4ª, agora é a 3ª
    ID,          # Coluna que era a 1ª, agora é a 4ª
    everything() # Todas as outras colunas na ordem original
  )

# --- Visualize o dataframe reordenado ---
print("\nDataframe APÓS a reordenação:")
View(data_final_pt_reordenado)
print(head(data_final_pt_reordenado))
print(colnames(data_final_pt_reordenado))

# Se você quiser que a variável original 'data_final_pt' seja atualizada com a nova ordem:
data_final_pt <- data_final_pt_reordenado

#saveRDS(data_final_pt, file = "data_final_pt.rds")
#data_final_pt<-readRDS(file = "data_final_pt.rds")
#====================================================================================================================
# Carregar os pacotes
library(tm)
library(SnowballC)

# --- 1. Criar o Corpus ---
# O Corpus será criado a partir das letras do dataframe df_musicas_portugues.
corpus_letras_pt <- VCorpus(VectorSource(df_musicas_portugues$Letra_Site))

print("Corpus de músicas em português criado com sucesso.")
print(paste0("Número de documentos (músicas) no corpus: ", length(corpus_letras_pt)))
print("Conteúdo original de uma amostra (primeira letra):")
print(corpus_letras_pt[[1]]$content)


# --- 2. Pré-processamento do Texto ---
# Aplicar as mesmas transformações de limpeza para garantir qualidade da DTM.

# 2.1. Converter para minúsculas
corpus_letras_pt <- tm_map(corpus_letras_pt, content_transformer(tolower))
# 2.2. Remover pontuação
corpus_letras_pt <- tm_map(corpus_letras_pt, removePunctuation)
# 2.3. Remover números
corpus_letras_pt <- tm_map(corpus_letras_pt, removeNumbers)
# 2.4. Remover Stop Words (palavras muito comuns que não agregam significado, ex: "o", "a", "de")
corpus_letras_pt <- tm_map(corpus_letras_pt, removeWords, stopwords("portuguese"))
# 2.5. Realizar Stemming (reduzir palavras à sua raiz, ex: "correr", "correndo" -> "corr")
#corpus_letras_pt <- tm_map(corpus_letras_pt, stemDocument, language = "portuguese")
# 2.6. Remover espaços em branco extras (deixa apenas um espaço entre as palavras)
corpus_letras_pt <- tm_map(corpus_letras_pt, stripWhitespace)

print("Pré-processamento do texto concluído.")
print("Conteúdo limpo de uma amostra (primeira letra após processamento):")
print(corpus_letras_pt[[1]]$content)
# --- 3. Criar a Matriz Documento-Termo (DTM) ---
# Usamos DocumentTermMatrix para criar a representação Bag-of-Words por frequência.
dtm_letras_pt <- DocumentTermMatrix(corpus_letras_pt)

print("Matriz Documento-Termo (DTM) para músicas em Português criada com sucesso (Bag-of-Words - Frequência).")

# Inspecionar a DTM
print("Dimensões da DTM (Documentos x Termos - Músicas x Palavras Únicas):")
print(dim(dtm_letras_pt))

# Para visualizar uma amostra da DTM (primeiras 5 linhas, primeiras 10 colunas, por exemplo)
# Convertendo para matriz densa para visualização, pode consumir muita memória para DTMs muito grandes.
dtm_matrix_sample_pt <- as.matrix(dtm_letras_pt[1:min(5, nrow(dtm_letras_pt)), 1:min(10, ncol(dtm_letras_pt))])
print("Amostra da DTM:")
print(dtm_matrix_sample_pt)
#===============================================================================================================
#pesquisar letras por palavras