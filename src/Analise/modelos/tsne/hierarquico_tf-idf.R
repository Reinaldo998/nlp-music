# Certifique-se de que as bibliotecas necessárias estão carregadas
library(Rtsne)
library(plotly)
library(htmlwidgets)
library(dplyr) # Para manipulação de dados
library(factoextra) # Para visualização do dendrograma

# ===================================
# PRÉ-REQUISITOS (certifique-se de que estas variáveis existem)
# ===================================
# As coordenadas t-SNE do seu código anterior (usando TF-IDF)
# tsne_tfidf$Y -> Contém as 2 dimensões do t-SNE para cada música

# As informações auxiliares das músicas
# nomes_musicas <- data_final_pt$Musica
# artistas <- data_final_pt$Artista.x
# anos <- data_final_pt$Ano
# num_palavras_tf <- rowSums(matriz_tf_t) # do seu código anterior


# ===================================
# 1. AGRUPAMENTO HIERÁRQUICO
# ===================================

cat("=== INICIANDO AGRUPAMENTO HIERÁRQUICO ===\n")

# Calcular a matriz de distâncias euclidianas das coordenadas t-SNE
# dist() calcula a distância entre as linhas da matriz
dist_matriz_tsne <- dist(tsne_tfidf$Y, method = "euclidean")

# Realizar o agrupamento hierárquico
# O método "ward.D2" é geralmente bom para criar clusters mais compactos e de tamanho semelhante
hc_resultado <- hclust(dist_matriz_tsne, method = "ward.D2")

cat("Agrupamento hierárquico concluído!\n")

# ===================================
# 2. VISUALIZAR O DENDROGRAMA (OPCIONAL)
# ===================================

cat("\n=== DENDROGRAMA DO AGRUPAMENTO HIERÁRQUICO ===\n")
# Você pode inspecionar o dendrograma para decidir quantos clusters cortar
plot(hc_resultado,
     main = "Dendrograma do Agrupamento Hierárquico (t-SNE TF-IDF)",
     xlab = "Músicas", ylab = "Altura (Distância)",
     hang = -1 # Para alinhar os rótulos na base
)

# Ou usar fviz_dend para um dendrograma mais bonito e interativo (requer ggplot2 e factoextra)
fviz_dend(hc_resultado,
          k = 5, # Exemplo: cortar para 5 clusters. Ajuste este valor após inspecionar o dendrograma.
          cex = 0.5, # Tamanho do texto
          palette = "jco", # Paleta de cores
          rect = TRUE, rect_border = "jco", rect_fill = TRUE, # Desenhar retângulos ao redor dos clusters
          main = "Dendrograma Agrupamento Hierárquico (t-SNE TF-IDF) com 5 Clusters"
)

cat("\nGerando dendrograma com fviz_dend (sem rótulos ou com rótulos minúsculos para velocidade):\n")

fviz_dend(hc_resultado,
          k = 5, # Exemplo: cortar para 5 clusters
          # cex = 0.001, # Use um valor extremamente pequeno se mesmo com labels=FALSE for lento
          labels_track_height = 0, # Altura da trilha dos rótulos (reduz espaço vertical)
          labels = FALSE, # **AQUI ESTÁ A CHAVE: Não desenha os rótulos das folhas**
          palette = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE,
          main = "Dendrograma (fviz_dend - Rápido) - Agrupamento Hierárquico (t-SNE TF)"
)

cat("\nSalvando dendrograma fviz_dend para um arquivo PDF (recomendado para muitos pontos):\n")

# Defina o nome do arquivo e as dimensões (ajuste conforme o número de músicas)
pdf("dendrograma_tsne_tf_fvizdend.pdf", width = 15, height = 10)

# Gere o gráfico dentro do dispositivo PDF
fviz_dend(hc_resultado,
          k = 5,
          cex = 0.05, # Ainda recomendo um cex pequeno para legibilidade em PDF com muitos pontos
          palette = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE,
          main = "Dendrograma (fviz_dend - PDF) - Agrupamento Hierárquico (t-SNE TF) com 5 Clusters"
)

dev.off() # IMPORTANTE: Fecha o dispositivo gráfico e salva o arquivo PDF
cat("Dendrograma fviz_dend salvo em 'dendrograma_tsne_tf_fvizdend.pdf'\n")

# Usando o resultado do agrupamento hierárquico (hc_resultado_tf ou hc_resultado_tfidf)
# AQUI ESTÁ A MUDANÇA: AGORA USAMOS tsne_muito_frequente$Y

cat("\nTentando plotar dendrograma novamente com 'plot()' e cex=0.01 (última tentativa visual rápida):\n")
plot(hc_resultado, # ou hc_resultado_tfidf
     main = "Dendrograma Rápido (t-SNE TF) - Estrutura Geral",
     xlab = "Músicas (rótulos ilegíveis para muitos pontos)",
     ylab = "Altura (Distância)",
     hang = -1,
     cex = 0.01 # Mantenha este valor MUITO pequeno para evitar o pior da lentidão
)
cat("Se o gráfico ainda demorar, é um limite da visualização para N alto.\n")
# ===================================
# 3. CORTAR O DENDROGRAMA E OBTER OS CLUSTERS
# ===================================

# Definir o número de clusters desejado.
# **Importante:** Altere 'k' para o número de clusters que faz sentido para seus dados,
#                 observando o dendrograma no passo anterior.
numero_de_clusters <- 5 # Exemplo: 5 clusters.

# Cortar o dendrograma para obter as atribuições de cluster para cada música
clusters_hc <- cutree(hc_resultado, k = numero_de_clusters)

cat("\nNúmero de clusters definidos:", numero_de_clusters, "\n")
cat("Distribuição dos clusters:\n")
print(table(clusters_hc))


# ===================================
# 4. INTEGRAR CLUSTERS NO DATAFRAME T-SNE E PLOTAR
# ===================================

cat("\n=== GERANDO GRÁFICO T-SNE COM CLUSTERS ===\n")

# Criar dataframe com coordenadas t-SNE e metadados, incluindo os clusters
df_tsne_com_clusters <- data.frame(
  TSNE1 = tsne_tfidf$Y[, 1],
  TSNE2 = tsne_tfidf$Y[, 2],
  Musica = nomes_musicas,
  Artista = artistas,
  Ano = anos,
  NumPalavras = num_palavras_tf,
  Cluster = factor(clusters_hc) # Transformar em fator para cores categóricas
)

# Criar texto para hover (incluindo o cluster)
hover_text_cluster <- paste(
  "Música:", df_tsne_com_clusters$Musica,
  "<br>Artista:", ifelse(is.na(df_tsne_com_clusters$Artista), "N/A", df_tsne_com_clusters$clusters_hc),
  "<br>Ano:", ifelse(is.na(df_tsne_com_clusters$Ano), "N/A", df_tsne_com_clusters$Ano),
  "<br>Total de palavras:", ifelse(is.na(df_tsne_com_clusters$NumPalavras), "N/A", df_tsne_com_clusters$NumPalavras),
  "<br>Cluster:", df_tsne_com_clusters$Cluster
)

# Gráfico t-SNE colorido pelos clusters hierárquicos
grafico_tsne_clusters <- plot_ly(
  data = df_tsne_com_clusters,
  x = ~TSNE1,
  y = ~TSNE2,
  text = hover_text_cluster,
  type = 'scatter',
  mode = 'markers',
  color = ~Cluster, # Colore por cluster
  colors = "Paired", # Paleta de cores para categorias (ajuste se precisar de mais cores)
  marker = list(
    size = 10,
    opacity = 0.8,
    line = list(width = 1, color = "white")
  ),
  hovertemplate = paste(
    "%{text}",
    "<extra></extra>"
  )
) %>%
  layout(
    title = list(
      text = paste0("t-SNE: Músicas Agrupadas Hierarquicamente (", numero_de_clusters, " Clusters)"),
      font = list(size = 18, color = "#2c3e50")
    ),
    xaxis = list(
      title = "t-SNE Componente 1",
      titlefont = list(size = 14),
      showgrid = TRUE, gridcolor = "#ecf0f1", zeroline = FALSE
    ),
    yaxis = list(
      title = "t-SNE Componente 2",
      titlefont = list(size = 14),
      showgrid = TRUE, gridcolor = "#ecf0f1", zeroline = FALSE
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode = "closest",
    font = list(family = "Arial, sans-serif")
  )

# Exibir gráfico
print(grafico_tsne_clusters)

# ===================================
# 5. SALVAR GRÁFICO (OPCIONAL)
# ===================================

arquivo_tsne_clusters <- paste0("tsne_hc_clusters_", numero_de_clusters, ".html")
saveWidget(
  widget = grafico_tsne_clusters,
  file = arquivo_tsne_clusters,
  selfcontained = TRUE,
  title = paste0("t-SNE Músicas com ", numero_de_clusters, " Clusters")
)
cat("\nGráfico t-SNE com clusters salvo em:", arquivo_tsne_clusters, "\n")

# ===================================
# 6. ANÁLISE DOS CLUSTERS (EXEMPLO)
# ===================================

cat("\n=== ANÁLISE DOS CLUSTERS ===\n")
# Adicionar a coluna de cluster ao dataframe original ou a um clone para análise
data_com_clusters <- data.frame(
  Musica = nomes_musicas,
  Artista = artistas,
  Ano = anos,
  Cluster = factor(clusters_hc)
)

# Exemplo: Contagem de músicas por cluster
cat("\nContagem de músicas por cluster:\n")
print(table(data_com_clusters$Cluster))

# Exemplo: Média do ano por cluster (para ver se há tendência temporal)
cat("\nMédia do ano por cluster:\n")
print(aggregate(Ano ~ Cluster, data = data_com_clusters, FUN = mean))

# Exemplo: Top 5 artistas por cluster (os mais frequentes)
cat("\nTop 5 Artistas por Cluster:\n")
data_com_clusters %>%
  group_by(Cluster, Artista) %>%
  summarise(N = n(), .groups = 'drop') %>%
  arrange(Cluster, desc(N)) %>%
  group_by(Cluster) %>%
  slice_head(n = 5) %>%
  print()

# Exemplo: Palavras mais frequentes em cada cluster (requer mais código para linkage)
# Isso é mais complexo e exigiria voltar à matriz de termos filtrada e agregar por cluster,
# mas é o próximo passo lógico para entender o "tópico" de cada cluster.

cat("\nAnálise de agrupamento hierárquico baseada em t-SNE concluída!\n")