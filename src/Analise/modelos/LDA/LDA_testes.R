#install.packages('topicmodels')
library(topicmodels)

# Carregar a dtm_filtrada do arquivo RDS
dtm_filtrada <- readRDS("dtm_filtrada.rds")

# --- 0. Carregar Pacotes Necessários (Execute apenas uma vez no início da sessão) ---
library(topicmodels) # Para LDA
library(dplyr)       # Para manipulação de dados
library(tidyr)       # Para pivot_wider
library(purrr)       # Para reduce (usado com full_join)
# library(ggplot2)   # Se for gerar gráficos de proporção de novo, mantenha carregado

# --- 1. Definir o Novo Número de Tópicos e Carregar a DTM Filtrada ---
k_novo <- 10 # <-- ALTERE AQUI PARA O NOVO NÚMERO DE GRUPOS (ex: 10)

# Carregar a dtm_filtrada que você salvou anteriormente (se estiver em um novo script)
# dtm_filtrada <- readRDS("dtm_filtrada.rds")
# Certifique-se que data_final_pt está carregado/disponível em sua sessão
# Ex: data_final_pt <- readRDS("seu_data_final_pt.rds") # Se você salvou

# --- 2. Ajustar o Novo Modelo LDA ---
# A variável será ap_lda_novo_k para não sobrescrever ap_lda_filtrada (com k=2)
ap_lda_novo_k <- LDA(dtm_filtrada, k = k_novo, method = "Gibbs")

# --- 3. Obter Atribuições de Tópicos e Combinar com os Dados Originais ---
# Novo data.frame com a atribuição de tópicos para o novo modelo
prob_documento_topico_novo <- posterior(ap_lda_novo_k)$topics
atribuicao_topico_novo <- as.data.frame(prob_documento_topico_novo) %>%
  mutate(topico_mais_provavel = max.col(.)) %>%
  select(topico_mais_provavel)


#==========================================================================================================================================================

# Criar um novo data.frame com a coluna de tópicos atribuídos
# A variável será data_com_topicos_novo para não sobrescrever data_com_topicos (com k=2)
data_com_topicos_novo <- data_final_pt %>%
  mutate(topico_lda = atribuicao_topico_novo$topico_mais_provavel)

# --- 4. Análise Resumida dos Grupos (Músicas por grupo e Média de Ano) ---
cat("\n\n=============================================\n")
cat(paste0("   ANÁLISE DOS GRUPOS (K = ", k_novo, ")\n"))
cat("=============================================\n")

analise_grupos_resumo <- data_com_topicos_novo %>%
  group_by(topico_lda) %>%
  summarise(
    num_musicas = n(),
    media_ano = round(mean(Ano, na.rm = TRUE)) # Assumindo 'Ano' é a coluna do ano
  ) %>%
  arrange(topico_lda)

print(analise_grupos_resumo)

# Opcional: Para ver os top termos de cada um dos k_novo tópicos
cat("\n\n=============================================\n")
cat(paste0("   TOP 10 TERMOS POR TÓPICO (K = ", k_novo, ")\n"))
cat("=============================================\n")
ap_top_terms_novo_k <- terms(ap_lda_novo_k, 10)
print(ap_top_terms_novo_k)


# --- 5. Tabela Alinhada com Top 10 Artistas por Grupo ---
cat("\n\n=============================================\n")
cat(paste0(" ARTISTAS MAIS FREQUENTES POR TÓPICO (K = ", k_novo, ", ALINHADA)\n"))
cat("=============================================\n")

lista_artistas_novo_k_pivot <- list()

for (i in 1:k_novo) {
  musicas_no_topico_atual <- data_com_topicos_novo %>%
    filter(topico_lda == i)
  
  artistas_por_topico_atual <- musicas_no_topico_atual %>%
    group_by(Artista.y) %>% # Assumindo 'Artista.y' é a coluna do artista
    summarise(contagem = n()) %>%
    arrange(desc(contagem)) %>%
    head(10) %>%
    mutate(posicao = row_number()) %>%
    select(posicao, Artista.y)
  
  colnames(artistas_por_topico_atual)[colnames(artistas_por_topico_atual) == "Artista.y"] <- paste0("Tópico ", i)
  lista_artistas_novo_k_pivot[[i]] <- artistas_por_topico_atual
}

tabela_artistas_alinhada_novo_k <- lista_artistas_novo_k_pivot %>%
  reduce(full_join, by = "posicao") %>%
  arrange(posicao)

View(tabela_artistas_alinhada_novo_k)

# Opcional: Visualizar o data.frame completo com os novos tópicos
# View(data_com_topicos_novo)

View(data_com_topicos_novo %>%
       dplyr::filter(topico_lda == 1))
