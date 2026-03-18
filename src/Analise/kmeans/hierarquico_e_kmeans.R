set.seed(100)
###################################################################################################
############# Hierárquico ############
dist_jsd <- as.dist(matriz_dist_jsd)                   
                   
                   
                   # 2. Executar o agrupamento
clus_hierarquico <- hclust(dist_jsd, method = "ward.D2")
                   
plot(clus_hierarquico, main = "Dendograma", 
     xlab = "Músicas", ylab = "Distância JSD", cex = 0.5)
################################################################
if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
library(factoextra)
library(ggplot2)

if (!requireNamespace("dendextend", quietly = TRUE)) install.packages("dendextend")
library(dendextend)
library(dplyr)


dend <- as.dendrogram(clus_hierarquico)

# "Podar" a árvore para mostrar apenas, por exemplo, os últimos 5 níveis de junção
# Isso torna a renderização instantânea e o gráfico muito mais legível.
dend_podado <- color_branches(dend, k = n_grupos)

cat("Renderizando versão podada (rápida e legível)...\n")

# Plotagem usando o motor base (muito mais rápido que ggplot)
# Mas com estética profissional do dendextend
plot(dend_podado, 
     leaflab = "none", # Remove etiquetas das folhas para clareza
     main = "Dendograma ",#Hierárquica dos Grupos (Vista Simplificada)
     ylab = "Distância JSD (Ward.D2)")

# Adiciona os retângulos dos grupos
if (!requireNamespace("dendextend", quietly = TRUE)) install.packages("dendextend")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")

library(dendextend)
library(viridis)
library(dplyr)

# --- Supondo que 'clus_hierarquico' e 'n_grupos' (44) já existem ---
# clus_hierarquico <- hclust(dist_jsd, method = "ward.D2")
# n_grupos <- 44 

# 2. Converter para objeto dendrograma
dend <- as.dendrogram(clus_hierarquico)

# 3. Aplicar Cores com a Paleta Viridis
# O viridis(n_grupos) gera uma lista de 44 cores únicas e harmônicas
cores_viridis <- viridis(n_grupos)

dend_colorido <- dend %>% 
  color_branches(k = n_grupos, col = cores_viridis) %>% 
  set("branches_lwd", 0.6) # Linhas levemente mais grossas para destacar as cores

cat("Renderizando dendrograma com paleta Viridis para", n_grupos, "grupos...\n")

# 4. Plotagem (Motor Base - Alta Performance)
# Usamos o plot base porque para 44 grupos o ggplot ficaria muito lento
plot(dend_colorido, 
     leaflab = "none", # Remove nomes das músicas para evitar poluição
     main = "Dendrograma ",
     ylab = "Distância JSD (Ward.D2)")

# 5. Linha de Corte (Opcional - ajuda a explicar o texto da dissertação)
abline(h = 1.4, col = "red", lty = 2, lwd = 1.5)


###############################################################
altura_corte <- 1.4 
abline(h = altura_corte, col = "red", lty = 2, lwd = 2)
grupos_gerados <- cutree(clus_hierarquico, h = altura_corte)
n_grupos <- length(unique(grupos_gerados))

legend("topright", legend = paste("Corte em h =", altura_corte, "\nResulta em", n_grupos, "grupos"), 
       bty = "n", text.col = "red")

cat("O corte na altura", altura_corte, "gerou", n_grupos, "grupos.\n")

######################################################################
############## kmeans ####################

k_escolhido <- n_grupos 
dim(dtm_tf_normalizada)
matriz_kmeans <- as.matrix(dtm_tf_normalizada)
set.seed(100) 

clusters_kmeans <- kmeans(matriz_para_kmeans, 
                          centers = k_escolhido, 
                          nstart = 25, 
                          iter.max = 100)

# 5. Interpretando os Grupos (Top Termos por Cluster)
#===================================================================
data_kmeans <- data_dp

# Troca a última coluna (que era o agrupamento do DP) pelo agrupamento do K-means
# A lógica presume que a última coluna de data_dp é o identificador do grupo anterior
data_kmeans[[ncol(data_kmeans)]] <- clusters_kmeans$cluster

# Renomeia a coluna para facilitar a identificação
colnames(data_kmeans)[ncol(data_kmeans)] <- "cluster_kmeans"

cat("K-means executado com sucesso utilizando a dtm_tf_normalizada.\n")
cat("Número de grupos (k):", k_escolhido, "\n")
cat("Novo dataframe 'data_kmeans' criado com os grupos atualizados.\n")
View(data_kmeans)
#===================================================================
# 5. Análise de Conteúdo: Top Palavras por Grupo
#===================================================================
library(dplyr)
library(tidyr)

# Resumo para entender o perfil léxico de cada grupo
top_termos_clusters <- as.data.frame(matriz_kmeans) %>%
  mutate(cluster = clusters_kmeans$cluster) %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(cols = -cluster, names_to = "termo", values_to = "freq_relativa_media") %>%
  group_by(cluster) %>%
  slice_max(freq_relativa_media, n = 10) %>%
  arrange(cluster, desc(freq_relativa_media))

cat("\n--- Top 10 Termos mais representativos por Grupo (K-means) ---\n")
print(top_termos_clusters)

#===================================================================
# 6. Análise de Artistas: Top Artistas por Grupo
#===================================================================

# Identifica quais artistas são mais frequentes em cada novo cluster
top_artistas_clusters <- data_kmeans %>%
  group_by(cluster_kmeans, Artista.y) %>%
  summarise(contagem = n(), .groups = 'drop') %>%
  group_by(cluster_kmeans) %>%
  slice_max(contagem, n = 10) %>%
  arrange(cluster_kmeans, desc(contagem))

cat("\n--- Top 10 Artistas mais frequentes por Grupo (K-means) ---\n")
print(top_artistas_clusters)

#====================================================================
saveRDS(data_kmeans, file = "data_kmeans.rds")
View(data_dp)
