########################################################
#### reproduzindo novamente a criação da matriz

# Criando Corpus
corpus <- VCorpus(VectorSource(data_final_pt$Letra)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower) %>%
  tm_map(removeWords, stopwords("pt")) %>%
  tm_map(stripWhitespace)

# Criando matriz TF-IDF
tdm_tfidf <- TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
matriz_tfidf <- as.matrix(tdm_tfidf)

# Filtrando termos muito raros
frequencias <- slam::row_sums(matriz_tfidf)
termos_frequentes <- names(frequencias[frequencias > 0])  # Exclui termos totalmente nulos

matriz_tfidf_filtrada <- matriz_tfidf[termos_frequentes, ]
matriz_tfidf_filtrada <- t(matriz_tfidf_filtrada)  # Transpõe para documentos como linhas
################################################################################################
set.seed(123)  # Para resultados reprodutíveis

# Aplica o k-means diretamente na matriz TF-IDF filtrada
kmeans_tfidf <- kmeans(matriz_tfidf_filtrada, centers = 7, nstart = 25)

# Adiciona os grupos ao dataframe original com as letras
data_final_pt$GrupoKmeans <- as.factor(kmeans_tfidf$cluster)

table(data_final_pt$GrupoKmeans)
#################################################################################
# Supondo que vc já tenha o resultado do kmeans feito em matriz_tfidf_filtrada:
# kmeans_tfidf <- kmeans(matriz_tfidf_filtrada, centers = 7, nstart = 25)

# Verifica se você está usando esse nome no seu dataset:
# Se for "data_final_pt", mude abaixo também para esse nome

# Garantir que o cluster foi adicionado corretamente
data_clusterizado <- data_final_pt
data_clusterizado$Cluster <- as.factor(kmeans_tfidf$cluster)

# Cria um novo data.frame com apenas as colunas desejadas
df_exportar <- data.frame(
  ID = seq_len(nrow(data_clusterizado)),
  Nome = data_clusterizado$Nome,
  Artista = data_clusterizado$Artista,
  Ano = data_clusterizado$Ano,
  Cluster = data_clusterizado$Cluster
)

# Visualiza as primeiras linhas
head(df_exportar)

# Garantir que o cluster foi adicionado corretamente
data_clusterizado <- data_final_pt
data_clusterizado$Cluster <- as.factor(kmeans_tfidf$cluster)


# Selecionar colunas na ordem desejada, mantendo o ID original
df_exportar <- data.frame(
  ID = data_clusterizado$ID,
  Nome = data_clusterizado$Nome,
  Artista = data_clusterizado$Artista,
  Ano = data_clusterizado$Ano,
  Cluster = data_clusterizado$Cluster
)

# Visualiza as primeiras linhas
head(df_exportar)

#write.csv(df_exportar, "/home/daiane/Reinaldo/musicas_clusterizadas_tfidf.csv", row.names = FALSE)

