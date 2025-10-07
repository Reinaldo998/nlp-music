# Certifique-se de que as bibliotecas necessárias estão carregadas
library(Rtsne)
library(plotly)
library(htmlwidgets)
library(dplyr) # Para manipulação de dados
library(factoextra) # Para visualização do dendrograma

# ===================================
# PRÉ-REQUISITOS (certifique-se de que estas variáveis existem)
# ===================================
# As coordenadas t-SNE do seu código anterior (usando TF da parte de palavras frequentes)
# AQUI ESTÁ A MUDANÇA: AGORA USAMOS tsne_muito_frequente$Y
tsne_data_for_hc <- tsne_muito_frequente$Y

# As informações auxiliares das músicas
# nomes_musicas <- data_final_pt$Musica
# artistas <- data_final_pt$Artista.x
# anos <- data_final_pt$Ano
# num_palavras_tf <- rowSums(matriz_tf_t) # do seu código anterior


# ===================================
# 1. AGRUPAMENTO HIERÁRQUICO
# ===================================

cat("=== INICIANDO AGRUPAMENTO HIERÁRQUICO (Baseado em t-SNE TF) ===\n")

# Calcular a matriz de distâncias euclidianas das coordenadas t-SNE
# dist() calcula a distância entre as linhas da matriz
dist_matriz_tsne_tf <- dist(tsne_data_for_hc, method = "euclidean")

# Realizar o agrupamento hierárquico
# O método "ward.D2" é geralmente bom para criar clusters mais compactos e de tamanho semelhante
hc_resultado_tf <- hclust(dist_matriz_tsne_tf, method = "ward.D2")

cat("Agrupamento hierárquico concluído!\n")

# ===================================
# 2. VISUALIZAR O DENDROGRAMA (OPCIONAL)
# ===================================

cat("\n=== DENDROGRAMA DO AGRUPAMENTO HIERÁRQUICO (t-SNE TF) ===\n")
# Você pode inspecionar o dendrograma para decidir quantos clusters cortar
plot(hc_resultado_tf, # Usando o resultado do TF
     main = "Dendrograma do Agrupamento Hierárquico (t-SNE TF)",
     xlab = "Músicas", ylab = "Altura (Distância)",
     hang = -1 # Para alinhar os rótulos na base
)

# --- A PARTE QUE PODE DEMORAR MUITO: fviz_dend ---
# Use com cautela para muitos pontos!
# Se você tiver muitas músicas, esta parte pode levar MUITO tempo ou consumir muita memória.
# Considere gerar para um arquivo PDF em vez de exibir no RStudio diretamente para grandes datasets.
cat("\nGerando dendrograma interativo (pode demorar para muitos pontos)...\n")
fviz_dend(hc_resultado_tf, # Usando o resultado do TF
          k = 5, # Exemplo: cortar para 5 clusters. Ajuste este valor após inspecionar o dendrograma.
          cex = 0.05, # Tamanho do texto - diminuído para muitos pontos
          palette = "jco", # Paleta de cores
          rect = TRUE, rect_border = "jco", rect_fill = TRUE, # Desenhar retângulos ao redor dos clusters
          main = "Dendrograma Agrupamento Hierárquico (t-SNE TF) com 5 Clusters"
)
cat("Dendrograma interativo gerado.\n")


cat("\nGerando dendrograma com fviz_dend (sem rótulos ou com rótulos minúsculos para velocidade):\n")

fviz_dend(hc_resultado_tf,
          k = 5, # Exemplo: cortar para 5 clusters
          # cex = 0.001, # Use um valor extremamente pequeno se mesmo com labels=FALSE for lento
          labels_track_height = 0, # Altura da trilha dos rótulos (reduz espaço vertical)
          labels = FALSE, # **AQUI ESTÁ A CHAVE: Não desenha os rótulos das folhas**
          palette = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE,
          main = "Dendrograma (fviz_dend - Rápido) - Agrupamento Hierárquico (t-SNE TF)"
)
# --- FIM DA PARTE QUE PODE DEMORAR ---

# ===================================
# 3. CORTAR O DENDROGRAMA E OBTER OS CLUSTERS
# ===================================

# Definir o número de clusters desejado.
# **Importante:** Altere 'k' para o número de clusters que faz sentido para seus dados,
#                 observando o dendrograma no passo anterior.
numero_de_clusters_tf <- 5 # Exemplo: 5 clusters.

# Cortar o dendrograma para obter as atribuições de cluster para cada música
clusters_hc_tf <- cutree(hc_resultado_tf, k = numero_de_clusters_tf)

cat("\nNúmero de clusters definidos (TF):", numero_de_clusters_tf, "\n")
cat("Distribuição dos clusters (TF):\n")
print(table(clusters_hc_tf))


# ===================================
# 4. INTEGRAR CLUSTERS NO DATAFRAME T-SNE E PLOTAR
# ===================================

cat("\n=== GERANDO GRÁFICO T-SNE (TF) COM CLUSTERS ===\n")

# Criar dataframe com coordenadas t-SNE e metadados, incluindo os clusters
df_tsne_tf_com_clusters <- data.frame(
  TSNE1 = tsne_data_for_hc[, 1],
  TSNE2 = tsne_data_for_hc[, 2],
  Musica = nomes_musicas,
  Artista = artistas,
  Ano = anos,
  NumPalavras = num_palavras_tf,
  Cluster = factor(clusters_hc_tf) # Transformar em fator para cores categóricas
)

# Criar texto para hover (incluindo o cluster)
hover_text_cluster_tf <- paste(
  "Música:", df_tsne_tf_com_clusters$Musica,
  "<br>Artista:", ifelse(is.na(df_tsne_tf_com_clusters$Artista), "N/A", df_tsne_tf_com_clusters$Artista),
  "<br>Ano:", ifelse(is.na(df_tsne_tf_com_clusters$Ano), "N/A", df_tsne_tf_com_clusters$Ano),
  "<br>Total de palavras:", ifelse(is.na(df_tsne_tf_com_clusters$NumPalavras), "N/A", df_tsne_tf_com_clusters$NumPalavras),
  "<br>Cluster:", df_tsne_tf_com_clusters$Cluster
)

# Gráfico t-SNE colorido pelos clusters hierárquicos
grafico_tsne_clusters_tf <- plot_ly(
  data = df_tsne_tf_com_clusters,
  x = ~TSNE1,
  y = ~TSNE2,
  text = hover_text_cluster_tf,
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
      text = paste0("t-SNE (TF): Músicas Agrupadas Hierarquicamente (", numero_de_clusters_tf, " Clusters)"),
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
print(grafico_tsne_clusters_tf)

# ===================================
# 5. SALVAR GRÁFICO (OPCIONAL)
# ===================================

arquivo_tsne_clusters_tf <- paste0("tsne_hc_clusters_tf_", numero_de_clusters_tf, ".html")
saveWidget(
  widget = grafico_tsne_clusters_tf,
  file = arquivo_tsne_clusters_tf,
  selfcontained = TRUE,
  title = paste0("t-SNE TF Músicas com ", numero_de_clusters_tf, " Clusters")
)
cat("\nGráfico t-SNE (TF) com clusters salvo em:", arquivo_tsne_clusters_tf, "\n")

# ===================================
# 6. ANÁLISE DOS CLUSTERS (EXEMPLO PARA TF)
# ===================================

cat("\n=== ANÁLISE DOS CLUSTERS (TF) ===\n")
# Adicionar a coluna de cluster ao dataframe original ou a um clone para análise
data_com_clusters_tf <- data.frame(
  Musica = nomes_musicas,
  Artista = artistas,
  Ano = anos,
  Cluster = factor(clusters_hc_tf)
)

# Exemplo: Contagem de músicas por cluster
cat("\nContagem de músicas por cluster (TF):\n")
print(table(data_com_clusters_tf$Cluster))

# Exemplo: Média do ano por cluster (para ver se há tendência temporal)
cat("\nMédia do ano por cluster (TF):\n")
print(aggregate(Ano ~ Cluster, data = data_com_clusters_tf, FUN = mean))

# Exemplo: Top 5 artistas por cluster (os mais frequentes)
cat("\nTop 5 Artistas por Cluster (TF):\n")
data_com_clusters_tf %>%
  group_by(Cluster, Artista) %>%
  summarise(N = n(), .groups = 'drop') %>%
  arrange(Cluster, desc(N)) %>%
  group_by(Cluster) %>%
  slice_head(n = 5) %>%
  print()

cat("\nAnálise de agrupamento hierárquico baseada em t-SNE (TF) concluída!\n")