#data_dp<- readRDS("/home/reinaldo/data_dp_2.rds")
#===================================================================
# Instalação e Carregamento de Pacotes
#===================================================================
# Instala os pacotes necessários caso não estejam instalados
required_packages <- c("dplyr", "ggplot2", "scales", "plotly", "htmlwidgets", "tidyr", "purrr", "Matrix", "philentropy")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Carrega todos os pacotes
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(htmlwidgets)
library(tidyr)
library(purrr)
library(Matrix) # Para readMM
library(philentropy) # Para funções de distância, se for calcular JSD aqui

#
#===================================================================
# Leitura e Preparação dos Dados
# ASSUMIDO: 'data_dp', 'data_final_pt' e 'dtm_filtrada' já estão carregadas na memória.
# 'data_dp' deve conter a coluna 'Grupos_MM_DP' com os IDs dos tópicos.
# 'dtm_filtrada' deve ser a sua matriz Documento-Termo (DTM).
# 'data_final_pt' é o seu dataframe original de músicas.
#===================================================================

# Mapeando variáveis do contexto do usuário para o script:
# 'y_ap' do script será 'dtm_filtrada' do usuário.
# 'data_agrupado' do script será 'data_dp' do usuário.
# 'dados_musicas_orig' do script será 'data_final_pt' do usuário.

# Atribui a DTM carregada pelo usuário à variável interna 'y_ap'
y_ap <- dtm_filtrada

# Garante que 'y_ap' (dtm_filtrada) tenha nomes de colunas (vocabulário)
if (is.null(colnames(y_ap)) || length(colnames(y_ap)) == 0) {
  stop("A matriz 'dtm_filtrada' (y_ap) deve ter nomes de colunas (vocabulário). 
        Certifique-se de que o vocabulário foi atribuído corretamente à 'dtm_filtrada' antes de rodar este script.")
}

# A variável 'data_agrupado' no script será a própria 'data_dp' do usuário,
# que já contém os grupos e as informações das músicas.
# Garante que 'data_dp' (agora data_agrupado) tem a coluna 'ID' para o mapeamento JSD.
# Se a coluna 'ID' não existir, o script irá parar, pois ela é essencial.
if (!"ID" %in% colnames(data_dp)) {
  stop("A coluna 'ID' não foi encontrada em 'data_dp'. Esta coluna é essencial para mapear as músicas na matriz JSD.")
}

data_agrupado <- data_dp

# Assegura que os tipos de coluna estão corretos em 'data_agrupado' (data_dp do usuário)
data_agrupado <- data_agrupado %>%
  mutate(
    Ano = as.numeric(as.character(Ano)),
    Grupos_MM_DP = as.numeric(as.character(Grupos_MM_DP))
  )

# ASSUMIR OU CALCULAR MATRIZ_DIST_JSD:
# Se você JÁ tem a 'matriz_dist_jsd' pré-calculada e carregada:
# Certifique-se de que os nomes das linhas/colunas da matriz_dist_jsd são os IDs das músicas (data_agrupado$ID)
# Exemplo (substitua pela sua matriz real se já existir):
# matriz_dist_jsd <- sua_matriz_jsd_aqui

# Se você PRECISA calcular a matriz_dist_jsd a partir de y_ap (dtm_filtrada):
# IMPORTANTE: A função JSD exige que as linhas da matriz y_ap sejam as observações
# e as colunas as features (palavras). Além disso, precisa de counts ou frequências.
# A DTM já deve estar no formato correto (músicas x palavras).
#
# Para JSD, as linhas da matriz devem ser as distribuições de probabilidade (soma = 1).
# Se y_ap contém contagens, você pode normalizá-las:
# Adiciona um pequeno valor (pseudocount) para evitar log(0)
if (!exists("matriz_dist_jsd")) {
  cat("A matriz_dist_jsd não foi encontrada. Calculando JSD entre todas as músicas. Isso pode levar tempo...\n")
  
  y_ap_for_jsd <- y_ap + 1e-10 
  y_ap_for_jsd <- y_ap_for_jsd / rowSums(y_ap_for_jsd)
  y_ap_for_jsd[is.nan(y_ap_for_jsd)] <- 0 # Lida com linhas que somam zero (ex: documentos vazios)
  
  # Define os nomes das linhas da DTM para corresponder aos IDs das músicas da coluna 'ID'
  # Cria um novo vetor de nomes de linha baseado na coluna 'ID' de data_agrupado
  row.names(y_ap_for_jsd) <- as.character(data_agrupado$ID)
  
  # Calcula a matriz de distância JSD
  # IMPORTANTE: A função JSD do 'philentropy' espera que cada LINHA seja uma distribuição.
  matriz_dist_jsd <- philentropy::JSD(y_ap_for_jsd, unit = "log2")
  
  # Atribuir nomes de linha/coluna se não vierem da JSD diretamente
  colnames(matriz_dist_jsd) <- row.names(y_ap_for_jsd)
  row.names(matriz_dist_jsd) <- row.names(y_ap_for_jsd)
  
  cat("Cálculo da matriz_dist_jsd concluído.\n")
} else {
  cat("Usando a matriz_dist_jsd pré-existente.\n")
  # Garante que os nomes das linhas da matriz JSD correspondem à coluna 'ID' em data_agrupado
  # Isso é crucial para o mapeamento correto nas análises subsequentes.
  if (!all(as.character(data_agrupado$ID) %in% row.names(matriz_dist_jsd))) {
    stop("Os IDs das músicas em 'data_agrupado$ID' não correspondem aos nomes das linhas da 'matriz_dist_jsd'. 
          Verifique a consistência dos IDs.")
  }
}


#
#===================================================================
# 1. Análise de Coerência por Tópico com Distância de Jensen-Shannon (JSD)
#===================================================================
cat("\n\n=============================================\n")
cat(" Análise de Coerência por Tópico (JSD)\n")
cat("=============================================\n")

# Calcular todos os scores de DISTÂNCIA e armazenar em formato longo
resultados_distancia_jsd_longo <- list()
topicos_existentes <- sort(unique(data_agrupado$Grupos_MM_DP))

for (topico_id in topicos_existentes) {
  musicas_do_topico <- data_agrupado %>%
    filter(Grupos_MM_DP == topico_id)
  
  # Apenas continua se houver mais de uma música no tópico
  if (nrow(musicas_do_topico) > 1) {
    # EXTRAÇÃO OTIMIZADA: Pega a sub-matriz de distância da matriz TOTAL
    codigos_do_topico <- as.character(musicas_do_topico$ID) # Usa a coluna 'ID'
    
    # Certifique-se de que os códigos existem na matriz de distância
    codigos_validos_no_topico <- codigos_do_topico[codigos_do_topico %in% row.names(matriz_dist_jsd)]
    
    if (length(codigos_validos_no_topico) > 1) {
      matriz_do_topico_jsd <- matriz_dist_jsd[codigos_validos_no_topico, codigos_validos_no_topico, drop = FALSE]
      
      # Extrai a triangular superior da matriz (sem a diagonal) para pegar os pares únicos
      dist_pares_jsd <- matriz_do_topico_jsd[upper.tri(matriz_do_topico_jsd)]
      
      # Armazena os resultados na lista
      resultados_distancia_jsd_longo[[as.character(topico_id)]] <- tibble(
        topico = as.integer(topico_id),
        distancia = dist_pares_jsd
      )
    }
  }
}

# Combina todos os resultados de distância em um único dataframe longo
df_distancias_jsd_longo <- bind_rows(resultados_distancia_jsd_longo)

if (nrow(df_distancias_jsd_longo) == 0) {
  stop("Nenhum score de distância de Jensen-Shannon foi calculado. Verifique se os tópicos têm mais de uma música e IDs válidos na matriz de distância.")
}
cat("Extração de distâncias de Jensen-Shannon por tópico concluída.\n")

#
#===================================================================
# 2. Resumo da Distância de Jensen-Shannon por Tópico
#===================================================================
analise_distancia_jsd_resumo <- df_distancias_jsd_longo %>%
  group_by(topico) %>%
  summarise(
    num_pares = n(),
    media = mean(distancia),
    mediana = median(distancia),
    min = min(distancia),
    max = max(distancia),
    .groups = 'drop'
  ) %>%
  arrange(topico)

print("\n--- Resumo de Distância de Jensen-Shannon por Tópico ---\n")
print(analise_distancia_jsd_resumo)


#xtable(analise_distancia_jsd_resumo)
#
#===================================================================
# 3. Gerar Boxplot da Distância de Jensen-Shannon para os 10 Tópicos Mais Frequentes
#===================================================================
cat("\n--- Gerando Boxplot da Distância de Jensen-Shannon para os 10 Tópicos Mais Frequentes ---\n")

# Filtra o dataframe de distâncias para incluir APENAS os 10 tópicos mais frequentes
df_distancias_top_10 <- df_distancias_jsd_longo %>%
  filter(topico %in% top_10_topicos_por_musicas)

# Gerar Boxplot
p_jsd_boxplot <- ggplot(df_distancias_top_10, aes(x = factor(topico), y = distancia, fill = factor(topico))) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribuição da Distância de Jensen-Shannon por Tópico (Top 10 Mais Frequentes)",
    x = "Tópico",
    y = "Distância de Jensen-Shannon (0 = idêntico, 1 = diferente)",
    fill = "Tópico"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotação dos rótulos do eixo X
  )

print(p_jsd_boxplot)

# Opcional: Salvar o boxplot como arquivo de imagem
# ggsave(paste0(caminho_saida, "/boxplot_distancia_jsd_top10_dp.png"), plot = p_jsd_boxplot, width = 10, height = 6)
# cat(paste0("\nBoxplot salvo como: '", caminho_saida, "/boxplot_distancia_jsd_top10_dp.png'\n"))
