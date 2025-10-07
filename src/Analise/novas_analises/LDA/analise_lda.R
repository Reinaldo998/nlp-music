# --- Carregar os pacotes necessários ---
library(dplyr)
library(tidyr)
library(stringr)  # Para manipulação de strings, se necessário
library(purrr)    # Para a função reduce (combinar dataframes)

# --- Supondo que os seguintes objetos já estão na memória: ---
# - dtm_filtrada (DTM usada para o LDA)
# - k_atual (Número total de tópicos definido anteriormente, ex: 140)
# - ap_lda_atual (Modelo LDA ajustado com k_atual tópicos)
# - data_com_topicos_atual (Dataframe com as músicas e a atribuição de tópico LDA)

cat("Iniciando a análise para os 11 tópicos com mais músicas...\n\n")

# --- 1. Identificar os 11 Tópicos com Mais Músicas ---
# Calcula o número de músicas por tópico
contagem_musicas_por_topico <- data_com_topicos_atual %>%
  group_by(topico_lda) %>%
  summarise(num_musicas = n()) %>%
  arrange(desc(num_musicas))

# Seleciona os IDs dos 11 tópicos com mais músicas
topicos_mais_populosos <- head(contagem_musicas_por_topico, 11)$topico_lda

if (length(topicos_mais_populosos) == 0) {
  stop("Nenhum tópico encontrado ou nenhum tópico com músicas. Verifique seus dados.")
}

cat("Os 11 tópicos com mais músicas são:", paste(topicos_mais_populosos, collapse = ", "), "\n")

#=========================================================================

cat("Iniciando o cálculo do número de palavras por tópico LDA...\n")

# --- 1. Calcular o número total e único de palavras por música na DTM ---

# Verifica se 'dtm_filtrada' e 'data_com_topicos_atual' estão disponíveis
if (!exists("dtm_filtrada") || !exists("data_com_topicos_atual")) {
  stop("Os objetos 'dtm_filtrada' e/ou 'data_com_topicos_atual' não foram encontrados. Por favor, carregue-os.")
}

# Converte a DTM filtrada para uma matriz densa
dtm_matrix_dense <- as.matrix(dtm_hibrida)#(dtm_filtrada)

# Calcula a soma das linhas da DTM (total de ocorrências de palavras)
num_palavras_total_por_musica <- rowSums(dtm_matrix_dense)

# Calcula a quantidade de palavras ÚNICAS por música
# 1. Cria uma matriz binária: qualquer valor > 0 se torna 1
dtm_matrix_binary <- (dtm_matrix_dense > 0) * 1
# 2. Soma as linhas da matriz binária para obter o número de palavras únicas
num_palavras_unicas_por_musica <- rowSums(dtm_matrix_binary)

# Cria um dataframe temporário para mapear o Codigo da música ao número de palavras
df_num_palavras_dtm <- data.frame(
  ID = as.numeric(row.names(dtm_filtrada)), # Garante que ID é CHARACTER e corresponde ao ID em data_com_topicos_atual
  Num_Palavras_Total_DTM = num_palavras_total_por_musica, # Total de ocorrências
  Num_Palavras_Unicas_DTM = num_palavras_unicas_por_musica # Quantidade de palavras únicas
)

# Junta este dataframe com a base principal do LDA (data_com_topicos_atual)
# para ter a informação do tópico LDA
data_com_topicos_e_palavras_lda <- data_com_topicos_atual %>%
  left_join(df_num_palavras_dtm, by = "ID")


# --- 2. Calcular a média e a mediana de palavras (total e únicas) por Tópico LDA ---

media_palavras_por_topico_lda <- data_com_topicos_e_palavras_lda %>%
  group_by(topico_lda) %>% # Agrupa as músicas pelo seu tópico LDA
  summarise(
    Media_Palavras_Total_por_Musica = round(mean(Num_Palavras_Total_DTM, na.rm = TRUE), 2), # Média total
    Mediana_Palavras_Total_por_Musica = round(median(Num_Palavras_Total_DTM, na.rm = TRUE), 2), # Mediana total
    Media_Palavras_Unicas_por_Musica = round(mean(Num_Palavras_Unicas_DTM, na.rm = TRUE), 2), # Média de palavras únicas
    Mediana_Palavras_Unicas_por_Musica = round(median(Num_Palavras_Unicas_DTM, na.rm = TRUE), 2), # Mediana de palavras únicas
    .groups = "drop" # Remove o agrupamento após o summarise
  ) %>%
  # Opcional: Remover a linha NA se houver (para músicas sem tópico atribuído)
  filter(!is.na(topico_lda))


print("\n--- Número Médio e Mediano de Palavras (Total e Únicas) por Música em Cada Tópico LDA ---")
print(media_palavras_por_topico_lda)
dim(media_palavras_por_topico_lda)
dim(analise_grupos_resumo_atual)
cat("\nAnálise de número de palavras por tópico LDA concluída.\n")

resumo_palavras_2<-cbind(media_palavras_por_topico_lda[,1],analise_grupos_resumo_atual[,2],media_palavras_por_topico_lda[,2:5],analise_grupos_resumo_atual[,3])
resumo_palavras_2<-resumo_palavras_2%>%arrange(desc(num_musicas))

xtable(resumo_palavras_2)


# --- 2. Top 10 Termos para os Tópicos Selecionados ---
cat("\n\n=============================================\n")
cat("  TOP 10 TERMOS PARA OS 11 TÓPICOS MAIS POPULOSOS\n")
cat("=============================================\n")

# Obtém todos os termos do modelo LDA
all_top_terms <- terms(ap_lda_atual, 10)

# DEBUG: Imprime os nomes das colunas para ver o formato exato
print("Nomes das colunas em 'all_top_terms':")
print(colnames(all_top_terms))

# CORREÇÃO AQUI: Constrói os nomes das colunas com "Topic " + o ID do tópico
# e usa 'one_of' para selecionar essas colunas.
# 'trimws' é usado para garantir que não haja espaços extras inadvertidos no início/fim dos IDs.
nomes_das_colunas_a_selecionar <- paste0("Topic ", trimws(as.character(topicos_mais_populosos)))

# Seleciona as colunas correspondentes aos tópicos mais populosos
ap_top_terms_selecionados <- as.data.frame(all_top_terms) %>%
  select(one_of(nomes_das_colunas_a_selecionar))

#View(ap_top_terms_selecionados)
print((ap_top_terms_selecionados))

#xtable(ap_top_terms_selecionados)

# --- 3. Tabela Alinhada com Top 10 Artistas para os Tópicos Selecionados ---
cat("\n\n=============================================\n")
cat(" ARTISTAS MAIS FREQUENTES PARA OS 11 TÓPICOS MAIS POPULOSOS (ALINHADA)\n")
cat("=============================================\n")

lista_artistas_para_alinhada_selecionados <- list()

for (topico_id in topicos_mais_populosos) {
  musicas_no_topico_filtradas <- data_com_topicos_atual %>%
    filter(topico_lda == topico_id)
  
  artistas_por_topico_corrente <- musicas_no_topico_filtradas %>%
    group_by(Artista.x) %>%
    summarise(contagem = n()) %>%
    arrange(desc(contagem)) %>%
    head(10) %>%
    mutate(posicao = row_number()) %>%
    select(posicao, Artista.x)
  
  # Renomeia a coluna do artista para incluir o ID do tópico
  colnames(artistas_por_topico_corrente)[colnames(artistas_por_topico_corrente) == "Artista.x"] <- paste0("Tópico ", topico_id)
  lista_artistas_para_alinhada_selecionados[[as.character(topico_id)]] <- artistas_por_topico_corrente
}

# Combina todas as tabelas de artistas em um único dataframe alinhado
tabela_artistas_alinhada_selecionados <- lista_artistas_para_alinhada_selecionados %>%
  reduce(full_join, by = "posicao") %>%
  arrange(posicao)

View(tabela_artistas_alinhada_selecionados)

cat("\nAnálise concluída para os 11 tópicos mais populosos.\n")

xtable(tabela_artistas_alinhada_selecionados)
#================================================================================



#
#===================================================================
# Assumindo que os dataframes e variáveis necessários já estão na memória:
# - data_com_topicos_atual (dataframe com ID, topico_lda, Ano, etc.)
# - topicos_mais_populosos (o vetor com os IDs dos 11 tópicos mais frequentes)
#===================================================================

cat("Iniciando a preparação dos dados e geração do gráfico para os 11 tópicos mais populosos...\n")

# --- 1. Preparar os dados para os gráficos de proporção por ano (APENAS os 11 tópicos mais frequentes) ---

dados_filtrados_para_grafico <- data_com_topicos_atual %>%
  filter(topico_lda %in% topicos_mais_populosos)

# Calcular o total de músicas por ano a partir do dataframe ORIGINAL
# Isso garante que a proporção seja calculada em relação ao total de músicas daquele ano.
total_musicas_por_ano_lda <- data_com_topicos_atual %>%
  group_by(Ano) %>%
  summarise(total_musicas = n(), .groups = "drop")

# Contar músicas por tópico e ano, APENAS para os 11 tópicos selecionados
musicas_topico_ano_lda_filtrado <- dados_filtrados_para_grafico %>%
  group_by(Ano, topico_lda) %>%
  summarise(contagem_topico_ano = n(), .groups = "drop")

# Juntar os totais e calcular a proporção
dados_grafico_proporcao_lda <- musicas_topico_ano_lda_filtrado %>%
  left_join(total_musicas_por_ano_lda, by = "Ano") %>%
  mutate(proporcao = contagem_topico_ano / total_musicas)

print("Dados para o gráfico preparados com sucesso para os 11 tópicos.")

# --- 2. Gerar o gráfico estático ggplot2 ---

cat("\n\n=============================================\n")
cat(" GRÁFICO ESTÁTICO: PROPORÇÃO DOS 11 TÓPICOS MAIS FREQUENTES (LDA)\n")
cat("=============================================\n")

# Definir a paleta de cores consistentemente para os 11 tópicos
# Assegura que a ordem numérica dos tópicos na legenda seja respeitada
topicos_ordenados_para_cores <- sort(as.numeric(topicos_mais_populosos))
paleta_de_cores_lda <- scales::hue_pal()(length(topicos_ordenados_para_cores))
names(paleta_de_cores_lda) <- as.character(topicos_ordenados_para_cores)


p_estatico_lda <- ggplot(dados_grafico_proporcao_lda,
                         aes(x = Ano, y = proporcao)) +
  # Linhas coloridas por tópico, garantindo o agrupamento correto
  geom_line(aes(color = factor(topico_lda, levels = as.character(topicos_ordenados_para_cores)),
                group = factor(topico_lda, levels = as.character(topicos_ordenados_para_cores))),
            linewidth = 1) +
  # Pontos coloridos por tópico
  geom_point(aes(color = factor(topico_lda, levels = as.character(topicos_ordenados_para_cores))),
             size = 2) +
  labs(
    title = "Proporção dos 11 Tópicos LDA Mais Frequentes ao Longo dos Anos",
    x = "Ano",
    y = "Proporção de Músicas",
    color = "Tópico LDA" # Legenda de cor atualizada
  ) +
  scale_y_continuous(labels = scales::percent) + # Formata o eixo Y como porcentagem
  # Usar scale_color_manual para aplicar a paleta de cores consistente
  scale_color_manual(values = paleta_de_cores_lda,
                     breaks = as.character(topicos_ordenados_para_cores), # Garante a ordem numérica na legenda
                     limits = as.character(topicos_ordenados_para_cores)) + # Garante que apenas os tópicos selecionados apareçam na legenda
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

print(p_estatico_lda)

cat("\nGráfico de proporção por ano para os 11 tópicos LDA mais populosos gerado com sucesso.\n")

#=============================================================================

cat("Iniciando a preparação dos dados e geração do gráfico Plotly para os 11 tópicos mais populosos...\n")

# --- 1. Preparar os dados para os gráficos de proporção por ano (APENAS os 11 tópicos mais frequentes) ---

dados_filtrados_para_grafico <- data_com_topicos_atual %>%
  filter(topico_lda %in% topicos_mais_populosos)

# Calcular o total de músicas por ano a partir do dataframe ORIGINAL
# Isso garante que a proporção seja calculada em relação ao total de músicas daquele ano.
total_musicas_por_ano_lda <- data_com_topicos_atual %>%
  group_by(Ano) %>%
  summarise(total_musicas = n(), .groups = "drop")

# Contar músicas por tópico e ano, APENAS para os 11 tópicos selecionados
musicas_topico_ano_lda_filtrado <- dados_filtrados_para_grafico %>%
  group_by(Ano, topico_lda) %>%
  summarise(contagem_topico_ano = n(), .groups = "drop")

# Juntar os totais e calcular a proporção
dados_grafico_proporcao_lda <- musicas_topico_ano_lda_filtrado %>%
  left_join(total_musicas_por_ano_lda, by = "Ano") %>%
  mutate(proporcao = contagem_topico_ano / total_musicas)

print("Dados para o gráfico preparados com sucesso para os 11 tópicos.")

# --- 2. Gerar o gráfico interativo plotly ---

cat("\n\n=============================================\n")
cat(" GRÁFICO INTERATIVO: PROPORÇÃO DOS 11 TÓPICOS LDA MAIS FREQUENTES (PLOTLY)\n")
cat("=============================================\n")

# Definir a paleta de cores consistentemente para os 11 tópicos
# Assegura que a ordem numérica dos tópicos na legenda seja respeitada
topicos_ordenados_para_cores <- sort(as.numeric(topicos_mais_populosos))
paleta_de_cores_lda <- scales::hue_pal()(length(topicos_ordenados_para_cores))
names(paleta_de_cores_lda) <- as.character(topicos_ordenados_para_cores)


# Crie o objeto ggplot com a estética 'text' para o balão interativo
p_base_plotly_lda <- ggplot(dados_grafico_proporcao_lda,
                            aes(x = Ano, y = proporcao,
                                color = factor(topico_lda, levels = as.character(topicos_ordenados_para_cores)), # Garante ordem de cores
                                group = factor(topico_lda, levels = as.character(topicos_ordenados_para_cores)), # Garante agrupamento correto para linhas
                                # --- A estética 'text' é crucial para o balão interativo do Plotly ---
                                text = paste("Tópico: ", topico_lda,
                                             "<br>Ano: ", Ano,
                                             "<br>Proporção: ", scales::percent(proporcao, accuracy = 0.01)))) + # Adiciona accuracy para formato %
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Proporção dos 11 Tópicos LDA Mais Frequentes ao Longo dos Anos",
    x = "Ano",
    y = "Proporção de Músicas",
    color = "Tópico LDA" # Legenda de cor atualizada
  ) +
  scale_y_continuous(labels = scales::percent) + # Formata o eixo Y como porcentagem
  # Usar scale_color_manual para aplicar a paleta de cores consistente
  scale_color_manual(values = paleta_de_cores_lda,
                     breaks = as.character(topicos_ordenados_para_cores), # Garante a ordem numérica na legenda
                     limits = as.character(topicos_ordenados_para_cores)) + # Garante que apenas os tópicos selecionados apareçam na legenda
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

# Converter o objeto ggplot para um gráfico interativo plotly
plotly_lda_top11_proporcao_final_com_balao <- ggplotly(p_base_plotly_lda, tooltip = "text")

# Exibir o gráfico interativo
print(plotly_lda_top11_proporcao_final_com_balao)

# Opcional: Salvar o gráfico interativo em HTML
# O 'caminho_arquivo_html_lda' deve ser definido
#caminho_arquivo_html_lda <- "grafico_lda_top11_proporcao_interativo_hibrido.html"
#saveWidget(plotly_lda_top11_proporcao_final_com_balao, file = caminho_arquivo_html_lda, selfcontained = TRUE)
# cat(paste0("\nGráfico interativo salvo como: '", caminho_arquivo_html_lda, "'\n"))

cat("\nGráfico interativo Plotly para os 11 tópicos LDA mais populosos gerado com sucesso.\n")

#boxplot_similaridade_lda_
#===========================================================================
## --- Supondo que os dataframes e matrizes necessários já estão na memória ---
# - matriz_sim_cosseno_total (a matriz completa de similaridade entre todas as músicas)
# - data_com_topicos_atual (dataframe com ID, topico_lda, etc.)
# - topicos_mais_populosos (o vetor com os IDs dos 11 tópicos mais populosos)

# --- 1. Recriar um dataframe longo com TODAS as similaridades de pares de tópicos ---
# Esta lógica garante que todos os pares de similaridade cruzada existam.
cat("Calculando todas as similaridades de cosseno entre pares dos 11 tópicos mais populosos...\n")

lista_similaridades <- list()
# Usamos apenas os 11 tópicos mais populosos para esta análise
topicos_para_analise <- sort(topicos_mais_populosos)

for (topico_i in topicos_para_analise) {
  # Apenas pegar os IDs das músicas do tópico i
  codigos_topico_i <- data_com_topicos_atual %>%
    filter(topico_lda == topico_i) %>%
    pull(ID)
  
  for (topico_j in topicos_para_analise) { # Loop aninhado sobre os mesmos 11 tópicos
    # Apenas pegar os IDs das músicas do tópico j
    codigos_topico_j <- data_com_topicos_atual %>%
      filter(topico_lda == topico_j) %>%
      pull(ID)
    
    # Continuar se houver músicas em ambos os tópicos para comparação
    if (length(codigos_topico_i) > 0 & length(codigos_topico_j) > 0) {
      # Assegurar que os códigos são caracteres e existem na matriz de similaridade
      codigos_topico_i_validos <- as.character(codigos_topico_i)
      codigos_topico_j_validos <- as.character(codigos_topico_j)
      
      # Filtrar para apenas os códigos que existem na matriz de similaridade
      codigos_topico_i_presentes <- codigos_topico_i_validos[codigos_topico_i_validos %in% rownames(matriz_sim_cosseno_total)]
      codigos_topico_j_presentes <- codigos_topico_j_validos[codigos_topico_j_validos %in% rownames(matriz_sim_cosseno_total)]
      
      if (length(codigos_topico_i_presentes) > 0 & length(codigos_topico_j_presentes) > 0) {
        sub_matriz_sim <- matriz_sim_cosseno_total[codigos_topico_i_presentes, codigos_topico_j_presentes]
        
        # Extrair os scores de similaridade para o formato longo
        # Se i == j, extrai a triangular superior (similaridade interna)
        if (topico_i == topico_j) {
          # Apenas se houver mais de uma música no tópico para ter pares
          if (length(codigos_topico_i_presentes) > 1) {
            sim_scores <- sub_matriz_sim[upper.tri(sub_matriz_sim)]
          } else {
            sim_scores <- numeric(0) # Nenhuma similaridade interna para um único elemento
          }
        } else {
          # Se i != j, extrai todos os valores (similaridade externa)
          sim_scores <- as.vector(sub_matriz_sim)
        }
        
        # Adicionar os scores à lista, apenas se houver scores para adicionar
        if(length(sim_scores) > 0) {
          lista_similaridades[[paste(topico_i, topico_j, sep = "_")]] <- tibble(
            topico_1 = topico_i,
            topico_2 = topico_j,
            similaridade = sim_scores
          )
        }
      }
    }
  }
}

df_similaridades_completo_filtrado <- bind_rows(lista_similaridades)

if (nrow(df_similaridades_completo_filtrado) == 0) {
  stop("Nenhum score de similaridade foi calculado para os tópicos selecionados. Verifique a matriz de similaridade total e a atribuição de tópicos.")
}
cat("Dataframe de similaridades filtrado e completo criado com sucesso.\n")


# --- 2. Gerar e exibir um plot de boxplots para CADA tópico principal SELECIONADO ---
cat("\nGerando plots de similaridade por tópico para os 11 tópicos mais populosos...\n")

# Definir a Paleta de Cores e a Ordem dos Tópicos Globalmente
# As cores agora serão definidas apenas para os tópicos selecionados
paleta_de_cores_global <- scales::hue_pal()(length(topicos_para_analise))
names(paleta_de_cores_global) <- as.character(topicos_para_analise)

for (topico_principal in topicos_para_analise) {
  df_plot <- df_similaridades_completo_filtrado %>%
    filter(topico_1 == topico_principal) %>%
    mutate(
      topico_comparado_id = topico_2,
      # CORREÇÃO: Cria um rótulo dinâmico para cada eixo X
      rotulo = case_when(
        topico_1 == topico_2 ~ paste0("Tópico ", topico_principal, " (Interno)"),
        TRUE ~ paste0("Tópico ", topico_2)
      ),
      # Garante que 'topico_cor' é um fator com os níveis corretos para cores consistentes
      topico_cor = factor(as.character(topico_2), levels = as.character(topicos_para_analise))
    )
  
  if (nrow(df_plot) > 0) {
    # Criar um lookup para os rótulos do eixo X de forma robusta
    labels_lookup <- df_plot %>%
      distinct(topico_comparado_id, rotulo) %>%
      arrange(topico_comparado_id)
    
    p <- ggplot(df_plot, aes(x = factor(topico_comparado_id, levels = as.character(topicos_para_analise)), # Força a ordem do eixo X
                             y = similaridade,
                             fill = topico_cor)) +
      geom_boxplot(alpha = 0.7) +
      scale_x_discrete(labels = setNames(labels_lookup$rotulo, labels_lookup$topico_comparado_id),
                       limits = as.character(topicos_para_analise)) + # Garante que todos os 11 tópicos apareçam no eixo X
      scale_fill_manual(values = paleta_de_cores_global,
                        breaks = names(paleta_de_cores_global),
                        limits = as.character(topicos_para_analise)) + # Garante que a legenda inclua todos os 11 tópicos
      labs(
        title = paste0("Similaridade do Tópico ", topico_principal, " com Outros Tópicos Selecionados"),
        x = "Tópico Comparado",
        y = "Similaridade de Cosseno",
        fill = "Tópico"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    print(p)
  }
}
cat("\nTodos os plots foram gerados com cores, posições e rótulos consistentes, e a legenda em ordem numérica para os 11 tópicos mais populosos.\n")

#===================================================================
# Assumindo que os dataframes e matrizes necessários já estão na memória:
# - matriz_dist_jsd (a matriz completa de DISTÂNCIA JSD entre todas as músicas)
# - data_com_topicos_atual (dataframe com ID, topico_lda, etc. - sua base principal com os tópicos LDA)
# - topicos_mais_populosos (o vetor com os IDs dos 11 tópicos mais frequentes)
#===================================================================

cat("Iniciando a análise de Distância de Jensen-Shannon (JSD) entre pares dos 11 tópicos mais populosos...\n")

# --- 1. Calcular todos os scores de DISTÂNCIA JSD e armazenar em formato longo ---
# Esta lógica garante que todos os pares de distância cruzada existam.
lista_distancias_jsd <- list()
# Usamos apenas os 11 tópicos mais populosos para esta análise
topicos_para_analise <- sort(topicos_mais_populosos)

# Identificar IDs Válidos da matriz de distância
codigos_validos_matriz_jsd <- row.names(matriz_dist_jsd)


for (topico_i in topicos_para_analise) {
  # Apenas pegar os IDs das músicas do tópico i que são Códigos válidos na matriz JSD
  codigos_topico_i <- data_com_topicos_atual %>%
    filter(topico_lda == topico_i) %>%
    pull(ID) %>%
    as.character()
  
  # Filtrar para garantir que o código realmente existe na matriz de distância
  codigos_topico_i <- codigos_topico_i[codigos_topico_i %in% codigos_validos_matriz_jsd]
  
  for (topico_j in topicos_para_analise) { # Loop aninhado sobre os mesmos 11 tópicos
    codigos_topico_j <- data_com_topicos_atual %>%
      filter(topico_lda == topico_j) %>%
      pull(ID) %>%
      as.character()
    
    codigos_topico_j <- codigos_topico_j[codigos_topico_j %in% codigos_validos_matriz_jsd]
    
    if (length(codigos_topico_i) > 0 & length(codigos_topico_j) > 0) {
      # Extrai a sub-matriz de distância JSD
      sub_matriz_dist_jsd <- matriz_dist_jsd[codigos_topico_i, codigos_topico_j, drop = FALSE]
      
      if (topico_i == topico_j) {
        # Para o mesmo tópico, pegamos a triangular superior (distância interna)
        # E agora, também os IDs das músicas que formam esses pares
        
        # Apenas se houver mais de uma música no tópico para ter pares
        if (length(codigos_topico_i) > 1) {
          # Obtém os índices da triangular superior
          indices_upper_tri <- which(upper.tri(sub_matriz_dist_jsd), arr.ind = TRUE)
          
          # Extrai os scores de distância
          dist_scores <- sub_matriz_dist_jsd[upper.tri(sub_matriz_dist_jsd)]
          
          # Extrai os IDs das músicas para cada par
          id_musica_1 <- row.names(sub_matriz_dist_jsd)[indices_upper_tri[,1]]
          id_musica_2 <- colnames(sub_matriz_dist_jsd)[indices_upper_tri[,2]]
        } else {
          dist_scores <- numeric(0)
          id_musica_1 <- character(0)
          id_musica_2 <- character(0)
        }
      } else {
        # Para tópicos diferentes, pegamos todas as distâncias (distância cruzada)
        # E também os IDs das músicas que formam esses pares
        
        dist_scores <- as.vector(sub_matriz_dist_jsd)
        
        # Cria todas as combinações de IDs para os pares cruzados
        id_musica_1 <- rep(row.names(sub_matriz_dist_jsd), times = ncol(sub_matriz_dist_jsd))
        id_musica_2 <- rep(colnames(sub_matriz_dist_jsd), each = nrow(sub_matriz_dist_jsd))
      }
      
      # Adicionar os scores à lista, apenas se houver scores para adicionar
      if(length(dist_scores) > 0) {
        lista_distancias_jsd[[paste(topico_i, topico_j, sep = "_")]] <- tibble(
          topico_1 = topico_i,
          topico_2 = topico_j,
          ID_musica_1 = id_musica_1, # Nova coluna
          ID_musica_2 = id_musica_2, # Nova coluna
          distancia = dist_scores
        )
      }
    }
  }
}

df_distancias_jsd_completo_filtrado <- bind_rows(lista_distancias_jsd)

if (nrow(df_distancias_jsd_completo_filtrado) == 0) {
  stop("Nenhum score de distância JSD foi calculado para os tópicos selecionados. Verifique a matriz de distância total e a atribuição de tópicos.")
}
cat("Dataframe de distâncias JSD filtrado e completo criado com sucesso.\n")


# --- 2. Definir Paleta de Cores ---
# topicos_para_analise já contém os IDs dos 11 tópicos mais populosos
# Criar uma lista ordenada NUMERICAMENTE dos tópicos para a legenda
topicos_ordenados_numeric_para_legenda <- sort(as.numeric(topicos_para_analise))
topicos_ordenados_numeric_para_legenda <- as.character(topicos_ordenados_numeric_para_legenda)

# Definir a Paleta de Cores e a Ordem dos Tópicos Globalmente (agora para os Top 11)
paleta_de_cores_global <- scales::hue_pal()(length(topicos_ordenados_numeric_para_legenda))
names(paleta_de_cores_global) <- topicos_ordenados_numeric_para_legenda


# --- 3. Gerar e exibir os plots de boxplots para os TOP 11 tópicos ---
cat("\nGerando plots de Distância de Jensen-Shannon por tópico para os 11 tópicos mais populosos...\n")

for (topico_principal in topicos_para_analise) {
  df_plot <- df_distancias_jsd_completo_filtrado %>% # Usando o dataframe filtrado
    filter(topico_1 == topico_principal) %>%
    mutate(
      topico_comparado_id = topico_2,
      rotulo = case_when(
        topico_1 == topico_2 ~ paste0("Tópico ", topico_principal, " (Interno)"),
        TRUE ~ paste0("Tópico ", topico_2)
      ),
      topico_cor = factor(as.character(topico_2), levels = topicos_ordenados_numeric_para_legenda)
    )
  
  if (nrow(df_plot) > 0) {
    p <- ggplot(df_plot, aes(x = factor(topico_comparado_id),
                             y = distancia,
                             fill = topico_cor)) +
      geom_boxplot(alpha = 0.7) +
      scale_x_discrete(labels = setNames(df_plot$rotulo, df_plot$topico_comparado_id)) +
      labs(
        title = paste0("Distância JSD do Tópico ", topico_principal, " com Outros Tópicos Selecionados"),
        x = "Tópico Comparado",
        y = "Distância de Jensen-Shannon (0 = idêntico, 1 = diferente)",
        fill = "Tópico"
      ) +
      theme_minimal() +
      scale_fill_manual(values = paleta_de_cores_global,
                        breaks = topicos_ordenados_numeric_para_legenda,
                        limits = topicos_ordenados_numeric_para_legenda) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    print(p)
  }
}
cat("\nTodos os plots de Distância JSD foram gerados com cores, posições e rótulos consistentes, e a legenda em ordem numérica para os 11 tópicos mais populosos.\n")
