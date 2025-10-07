############3 pca
#install.packages("factoextra")
#install.packages("FactoMineR")
#install.packages("plotly")
library(dplyr)
library(tm)
library(slam)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(plotly)
# Pré-processamento das letras
# Vamos assumir que o df_final_pt já contém a coluna 'Letra' com as letras das músicas

# Limpeza do texto
corpus <- VCorpus(VectorSource(data_final_pt$Letra)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower) %>%
  tm_map(removeWords, stopwords("pt")) %>%
  tm_map(stripWhitespace)

# Criando a TermDocumentMatrix
corpus <- tolower(data_final_pt$Letra) # Isso transforma em um vetor de caracteres
tdm <- TermDocumentMatrix(corpus)    # Erro! corpus não é um VCorpus

# Transformar a TDM em uma matriz
matriz_tdm <- as.matrix(tdm)

# Verificando o número de termos
dim(matriz_tdm)
ncol(matriz_tdm)
# Aplicando PCA na matriz
pca_resultado <- prcomp(matriz_tdm, scale. = TRUE)

# Resumo do PCA
summary(pca_resultado)

# Visualizando o gráfico da variância explicada pelos componentes principais
fviz_eig(pca_resultado)

# Visualizando os componentes principais (gráfico de dispersão)
fviz_pca_ind(pca_resultado, 
             geom.ind = "point", 
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             label = "none") + coord_cartesian(xlim = c(-30, 5), ylim = c(-10, 15))

#===============================================================================================

library(dplyr)
library(tm)
library(slam)
library(FactoMineR) # Embora não usado no fviz_eig/fviz_pca_ind, é um pacote de PCA comum
library(factoextra)
library(ggplot2)
library(plotly) # Para o fviz_pca_ind que você usou

# --- CORREÇÃO 1: Definir o seed para reprodutibilidade ---
set.seed(123) # Boa prática para garantir que resultados que envolvem aleatoriedade sejam os mesmos a cada execução

# Pré-processamento das letras
# Vamos assumir que o data_final_pt já contém a coluna 'Letra' com as letras das músicas

# Limpeza do texto
# --- CORREÇÃO 2: Uso correto de tolower com VCorpus ---
# A função tolower precisa de um "content_transformer" para operar em um objeto Corpus
corpus <- VCorpus(VectorSource(data_final_pt$Letra)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>% # AQUI ESTÁ A CORREÇÃO!
  tm_map(removeWords, stopwords("pt")) %>%
  tm_map(stripWhitespace)

# Criando a TermDocumentMatrix (TDM)
# --- CORREÇÃO 3: REMOÇÃO DA LINHA PROBLEMÁTICA ---
# A linha 'corpus <- tolower(data_final_pt$Letra)' foi removida,
# pois ela sobrescrevia o objeto VCorpus e causava o erro.
# Agora 'corpus' continua sendo um VCorpus, que é o esperado por TermDocumentMatrix.
tdm <- TermDocumentMatrix(corpus)

# Transformar a TDM em uma matriz
matriz_tdm <- as.matrix(tdm)

# --- CORREÇÃO 4: Transpor a matriz para PCA de MÚSICAS ---
# Por padrão, TDM tem termos nas linhas e documentos nas colunas.
# Para fazer PCA de documentos (músicas), precisamos que as músicas sejam as linhas.
matriz_tdm_transposta <- t(matriz_tdm)

# Verificando o número de termos e documentos (agora na orientação correta)
cat("Dimensões da matriz para PCA (Músicas x Palavras): ")
print(dim(matriz_tdm_transposta))


# Aplicando PCA na matriz transposta
# É crucial que a matriz de entrada não contenha colunas com variância zero
# (palavras que aparecem em todas as músicas ou em nenhuma, após a limpeza).
# prcomp geralmente lida com isso, mas pode ser bom estar ciente.
pca_resultado <- prcomp(matriz_tdm_transposta, scale. = TRUE)

# Resumo do PCA
summary(pca_resultado)

# Visualizando o gráfico da variância explicada pelos componentes principais
cat("\nGráfico de Variância Explicada pelo PCA:\n")
fviz_eig(pca_resultado, main = "PCA TF - Variância Explicada (Músicas)") # Adicionei um título

# Visualizando os componentes principais (gráfico de dispersão)
# Agora, cada ponto representa uma MÚSICA.
cat("\nGráfico de Dispersão dos Componentes Principais (PCA de Músicas):\n")
fviz_pca_ind(pca_resultado,
             geom.ind = "point",
             col.ind = "cos2", # Colore os pontos pela qualidade da representação (cos²)
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             label = "none", # Não mostra rótulos dos pontos (para muitos pontos, fica poluído)
             title = "PCA - Músicas por Frequência de Termos (TF)" # Adicionei um título
) +
  coord_cartesian(xlim = c(-30, 5), ylim = c(-10, 15)) # Mantive os limites se fazem sentido para seus dados


################################################################################################
############ Agora com tf idf ############

# Instalação dos pacotes necessários (se ainda não tiver)
# install.packages("tm")
# install.packages("slam")
# install.packages("FactoMineR")
# install.packages("factoextra")
# install.packages("ggplot2")

library(dplyr)
library(tm)
library(slam)
library(FactoMineR)
library(factoextra)
library(ggplot2)

# Garantir que o texto está no formato certo
letras <- tolower(data_final_pt$Letra)

# Criação do corpus
corpus <- VCorpus(VectorSource(letras))

# Pré-processamento
corpus <- corpus %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("pt")) %>%
  tm_map(stripWhitespace)

# Criar a matriz Term-Document com pesos TF-IDF
dtm <- TermDocumentMatrix(corpus,
                          control = list(weighting = weightTfIdf))

# Converter para matriz numérica
matriz_tfidf <- as.matrix(dtm)

# Transpor para ficar como documentos x termos
matriz_tfidf_t <- t(matriz_tfidf)

# Remover colunas (termos) com variância zero
variancias <- apply(matriz_tfidf_t, 2, var)
matriz_filtrada <- matriz_tfidf_t[, variancias > 0]

# Aplicar PCA
pca_resultado <- prcomp(matriz_filtrada, scale. = TRUE)

# Resumo do PCA
summary(pca_resultado)

# Variância explicada
fviz_eig(pca_resultado)

pca_resultado2 <- PCA(matriz_tfidf_filtrada, scale.unit = FALSE, graph = FALSE)

# Gráfico dos documentos (letras de músicas)
grafico <-fviz_pca_ind(pca_resultado, 
             geom.ind = "point", 
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             label = "none")
grafico + coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2))

fviz_pca_ind(pca_resultado,
             geom = "point",
             col.ind = "cos2",     # intensidade com base na qualidade da representação
             gradient.cols = c("white", "blue", "red"),
             repel = TRUE)+ geom_text_repel(aes(label = rownames(pca_resultado$x)), 
                                           data = as.data.frame(pca_resultado$x),
                                           max.overlaps = 20)


# Gráfico das variáveis (termos mais relevantes)
fviz_pca_var(pca_resultado, 
             col.var = "contrib", 
             gradient.cols = c("blue", "red"), 
             repel = TRUE)

library(ggrepel)

pca_resultado$x
