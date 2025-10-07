# --- 2. Definir o Número de Tópicos para esta Análise ---
k_atual <- 100 # <--- DEFINE AQUI O NÚMERO DE TÓPICOS QUE VOCÊ DESEJA PARA ESTA EXECUÇÃO!

# --- 3. Ajustar o Modelo LDA ---
ap_lda_atual <- LDA(dtm_final_lda, k = k_atual, method = "Gibbs")

# --- 4. Obter Atribuições de Tópicos e Combinar com os Dados Originais (CÓDIGO CORRIGIDO) ---
prob_documento_topico_atual <- posterior(ap_lda_atual)$topics

# O resultado do posterior() tem os Codigos das músicas como row.names.
# A melhor forma de mapear é criar um dataframe a partir disso.
df_atribuicao_topico <- data.frame(
  Codigo = as.numeric(row.names(prob_documento_topico_atual)), # Pega os Códigos da DTM
  topico_prob = prob_documento_topico_atual,
  stringsAsFactors = FALSE
) %>%
  # Encontra o tópico mais provável para cada música
  mutate(topico_lda = max.col(prob_documento_topico_atual)) %>%
  select(Codigo, topico_lda)

#data_com_topicos_lda_seguro 
data_com_topicos_atual<- data_final_pt %>%
  left_join(df_atribuicao_topico, by = c("ID" = "Codigo"))


View(df_atribuicao_topico)
View(data_com_topicos_atual)
row.names(dtm_filtrada)

# --- 5. Análise Resumida dos Grupos (Músicas por grupo e Média de Ano) ---
cat("\n\n=============================================\n")
cat(paste0("   ANÁLISE DOS GRUPOS (K = ", k_atual, ")\n"))
cat("=============================================\n")

analise_grupos_resumo_atual <- data_com_topicos_atual %>%
  group_by(topico_lda) %>%
  summarise(
    num_musicas = n(),
    media_ano = round(mean(Ano, na.rm = TRUE))
  ) %>%
  arrange(topico_lda)

print(analise_grupos_resumo_atual)
View(analise_grupos_resumo_atual)
# Opcional: Top 10 termos para cada tópico do modelo atual
cat("\n\n=============================================\n")
cat(paste0("   TOP 10 TERMOS POR TÓPICO (K = ", k_atual, ")\n"))
cat("=============================================\n")
ap_top_terms_atual <- terms(ap_lda_atual, 10)
View(ap_top_terms_atual)


# --- 6. Tabela Alinhada com Top 10 Artistas por Grupo ---
cat("\n\n=============================================\n")
cat(paste0(" ARTISTAS MAIS FREQUENTES POR TÓPICO (K = ", k_atual, ", ALINHADA)\n"))
cat("=============================================\n")

lista_artistas_para_alinhada <- list()

for (i in 1:k_atual) {
  musicas_no_topico_filtradas <- data_com_topicos_atual %>%
    filter(topico_lda == i)
  
  artistas_por_topico_corrente <- musicas_no_topico_filtradas %>%
    group_by(Artista.x) %>%
    summarise(contagem = n()) %>%
    arrange(desc(contagem)) %>%
    head(10) %>%
    mutate(posicao = row_number()) %>%
    select(posicao, Artista.x)
  
  colnames(artistas_por_topico_corrente)[colnames(artistas_por_topico_corrente) == "Artista.x"] <- paste0("Tópico ", i)
  lista_artistas_para_alinhada[[i]] <- artistas_por_topico_corrente
}

tabela_artistas_alinhada_atual <- lista_artistas_para_alinhada %>%
  reduce(full_join, by = "posicao") %>%
  arrange(posicao)

View(tabela_artistas_alinhada_atual)

##===============================================================================

cat("Iniciando a análise para os 10 tópicos com mais músicas...\n\n")

# --- 1. Identificar os 10 Tópicos com Mais Músicas ---
# Calcula o número de músicas por tópico
contagem_musicas_por_topico <- data_com_topicos_atual %>%
  group_by(topico_lda) %>%
  summarise(num_musicas = n()) %>%
  arrange(desc(num_musicas))

View(contagem_musicas_por_topico)
# Seleciona os IDs dos 10 tópicos com mais músicas
topicos_mais_populosos <- head(contagem_musicas_por_topico, 11)$topico_lda

if (length(topicos_mais_populosos) == 0) {
  stop("Nenhum tópico encontrado ou nenhum tópico com músicas. Verifique seus dados.")
}

cat("Os 10 tópicos com mais músicas são:", paste(topicos_mais_populosos, collapse = ", "), "\n")


# --- 2. Top 10 Termos para os Tópicos Selecionados ---
cat("\n\n=============================================\n")
cat("  TOP 10 TERMOS PARA OS 10 TÓPICOS MAIS POPULOSOS\n")
cat("=============================================\n")

# Obtém todos os termos do modelo LDA
all_top_terms <- terms(ap_lda_atual, 10)

# Filtra apenas os termos dos tópicos mais populosos
# Convertemos 'all_top_terms' para um dataframe para facilitar a filtragem
ap_top_terms_selecionados <- as.data.frame(all_top_terms) %>%
  select(starts_with(paste0("Topic.", topicos_mais_populosos)))

View(ap_top_terms_selecionados)


# --- 3. Tabela Alinhada com Top 10 Artistas para os Tópicos Selecionados ---
cat("\n\n=============================================\n")
cat(" ARTISTAS MAIS FREQUENTES PARA OS 10 TÓPICOS MAIS POPULOSOS (ALINHADA)\n")
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

cat("\nAnálise concluída para os 10 tópicos mais populosos.\n")


