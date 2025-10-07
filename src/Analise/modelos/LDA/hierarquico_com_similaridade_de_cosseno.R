# Certifique-se de que os pacotes necessários estão carregados
library(lsa)   # Para a similaridade de cosseno
library(dplyr) # Para manipulação de dados

# Assumimos que 'matriz_similaridade_total' já está na sua sessão R
# com os IDs das músicas como nomes de linhas/colunas.
# E que 'data_final_pt' está carregado e tem a coluna 'ID'.

# --- 1. Calcular a Matriz de Distância de Cosseno ---
matriz_distancia_cosseno <- as.dist(1 - matriz_similaridade_total)

# --- 2. Aplicar o Agrupamento Hierárquico ---
k_clusters_desejado <- 10 # Por exemplo, 10 clusters (ajuste conforme necessário)
hc_resultado <- hclust(matriz_distancia_cosseno, method = "complete") # Use o método que preferir

# 'cutree()' atribui cada música a um cluster.
grupos_hierarquicos <- cutree(hc_resultado, k = k_clusters_desejado)

# --- 3. Criar um novo DataFrame para os resultados do agrupamento ---
# A coluna 'ID' em df_resultados_agrupamento_cosseno_temp será gerada a partir dos nomes do 'grupos_hierarquicos',
# que são os IDs das músicas da sua matriz de similaridade.
df_resultados_agrupamento_cosseno_temp <- data.frame(
  ID = names(grupos_hierarquicos),
  Cluster_Cosseno = grupos_hierarquicos,
  stringsAsFactors = FALSE
)

# --- CORREÇÃO DE TIPO DE DADO AQUI: Garantir que o ID seja numérico ---
# Isso é crucial para o join com data_final_pt.
df_resultados_agrupamento_cosseno_temp <- df_resultados_agrupamento_cosseno_temp %>%
  mutate(ID = as.numeric(ID))

# --- 4. Criar um NOVO DataFrame combinando data_final_pt com os resultados do cluster ---
# Esta é a variável NOVA que terá as informações dos clusters, sem alterar data_final_pt.
data_com_clusters_cosseno <- data_final_pt %>%
  left_join(df_resultados_agrupamento_cosseno_temp, by = "ID")

# --- 5. Resumo dos Clusters ---
cat(paste0("\n\n--- Resumo dos ", k_clusters_desejado, " Clusters por Similaridade de Cosseno ---\n"))
resumo_clusters_cosseno <- data_com_clusters_cosseno %>% # Usamos o NOVO DataFrame aqui
  group_by(Cluster_Cosseno) %>%
  summarise(
    Num_Musicas_Cluster = n(),
    Media_Ano_Cluster = round(mean(Ano, na.rm = TRUE)) # Supondo que 'Ano' está em data_final_pt
  ) %>%
  arrange(Cluster_Cosseno)

print(resumo_clusters_cosseno)

# --- Opcional: Visualizar o dendrograma ---
# plot(hc_resultado,
#      main = "Dendrograma de Agrupamento Hierárquico (Distância de Cosseno)",
#      xlab = "Músicas", ylab = "Distância",
#      hang = -1
# )
# library(ggdendro) # Para dendrogramas mais bonitos
# ggdendrogram(hc_resultado, rotate = FALSE, size = 2) +
#   labs(title = "Dendrograma de Agrupamento Hierárquico") +
#   theme_minimal()

# --- Opcional: Salvar resultados ---
# saveRDS(data_com_clusters_cosseno, file = "musicas_com_clusters_cosseno.rds")
# write.csv(resumo_clusters_cosseno, file = "resumo_clusters_cosseno.csv", row.names = FALSE)

# --- NOVA PARTE: Tabela Alinhada com Top 10 Artistas por Cluster de Similaridade de Cosseno ---

cat("\n\n=================================================================================\n")
cat(paste0(" ARTISTAS MAIS FREQUENTES NOS CLUSTERS DE SIMILARIDADE DE COSSENO (K = ", k_clusters_desejado, ", ALINHADA)\n"))
cat("=================================================================================\n")

# 1. Identificar os clusters para os quais queremos a tabela de artistas
# (Vamos pegar todos os clusters gerados aqui, mas você pode filtrar se quiser os top 10 maiores, por exemplo)
clusters_para_tabela_artistas <- sort(unique(data_com_clusters_cosseno$Cluster_Cosseno))

# Inicializar uma lista vazia para armazenar os data.frames de artistas de cada cluster
lista_artistas_clusters_cosseno <- list()

# 2. Iterar sobre cada cluster para encontrar os top artistas
for (i in clusters_para_tabela_artistas) {
  # Filtrar as músicas que pertencem ao cluster atual
  musicas_no_cluster_atual <- data_com_clusters_cosseno %>%
    filter(Cluster_Cosseno == i)
  
  # Calcular os top 10 artistas para este cluster
  artistas_por_cluster_corrente <- musicas_no_cluster_atual %>%
    group_by(Artista.y) %>% # Assumindo 'Artista.y' é a coluna do nome do artista
    summarise(contagem = n()) %>%
    arrange(desc(contagem)) %>%
    head(10) %>% # Pegar os top 10 artistas
    mutate(posicao = row_number()) %>% # Adicionar uma coluna de posição (1, 2, ..., 10)
    select(posicao, Artista.y) # Selecionar apenas a posição e o nome do artista
  
  # Renomear a coluna do artista para que seja o nome do cluster
  colnames(artistas_por_cluster_corrente)[colnames(artistas_por_cluster_corrente) == "Artista.y"] <- paste0("Cluster ", i)
  
  # Armazenar este data.frame na lista
  lista_artistas_clusters_cosseno[[as.character(i)]] <- artistas_por_cluster_corrente
}

# 3. Consolidar em uma única tabela alinhada
if (length(lista_artistas_clusters_cosseno) > 0) {
  tabela_artistas_clusters_cosseno_alinhada <- lista_artistas_clusters_cosseno %>%
    reduce(full_join, by = "posicao") %>%
    arrange(posicao) %>%
    mutate_all(~replace_na(., "")) # Preencher NAs com strings vazias para uma exibição mais limpa
  
  print(tabela_artistas_clusters_cosseno_alinhada)
  
  # Opcional: Visualizar em uma janela interativa no RStudio
  # View(tabela_artistas_clusters_cosseno_alinhada)
  
  # Opcional: Salvar a tabela alinhada
  # write.csv(tabela_artistas_clusters_cosseno_alinhada, "artistas_clusters_cosseno_alinhada.csv", row.names = FALSE)
  # saveRDS(tabela_artistas_clusters_cosseno_alinhada, "artistas_clusters_cosseno_alinhada.rds")
  
} else {
  cat("Não foram encontrados clusters ou dados suficientes para gerar a tabela alinhada de artistas.\n")
}