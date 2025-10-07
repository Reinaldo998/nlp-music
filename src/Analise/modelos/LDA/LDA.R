#install.packages("topicmodels")
library(topicmodels)

# Carregar a dtm_filtrada do arquivo RDS
dtm_filtrada <- readRDS("dtm_filtrada.rds")

# ===================================
# AJUSTANDO O MODELO LDA COM A DTM FILTRADA
# ===================================

k=10

# Utilizando a 'dtm_filtrada' como entrada para o modelo LDA
# 'k = 2' ainda indica que você quer encontrar 2 tópicos
ap_lda_filtrada <- LDA(dtm_filtrada, k = 10, method="Gibbs")

# ===================================
# TOP 10 TERMOS DE CADA TÓPICO DA MATRIZ FILTRADA
# ===================================

ap_top_terms_filtrada <- terms(ap_lda_filtrada, 10)

print(ap_top_terms_filtrada)


###################################################################

# Certifique-se de que os pacotes necessários estão carregados
library(topicmodels)
library(dplyr)
library(stringr) # Para manipulação de strings, se necessário para limpeza de nomes

# --- PASSO 1: Obter as atribuições de tópicos para cada música ---

# A função posterior() do modelo LDA dá as probabilidades de cada documento pertencer a cada tópico.
# $topics contém a matriz de probabilidades documento-tópico.
prob_documento_topico <- posterior(ap_lda_filtrada)$topics

# Para cada música, encontrar o tópico com a maior probabilidade
# A função 'max.col' retorna o índice da coluna com o valor máximo por linha
atribuicao_topico_por_musica <- as.data.frame(prob_documento_topico) %>%
  mutate(topico_mais_provavel = max.col(.)) %>%
  select(topico_mais_provavel)

# O resultado 'atribuicao_topico_por_musica' é um data.frame simples
# com uma coluna 'topico_mais_provavel' indicando o número do tópico (ex: 1 ou 2)
# para cada música, na ordem em que estavam no seu dtm_filtrada.

# --- PASSO 2: Combinar com os dados originais ---

# É crucial que a ordem das músicas em 'atribuicao_topico_por_musica'
# corresponda à ordem das músicas em 'data_final_pt'.
# O 'doc_id' na criação do corpus ajuda a manter essa ordem.

# Adicionar a coluna de tópico mais provável ao seu data.frame original
# Certifique-se de que 'data_final_pt' é o seu data.frame original.
data_com_topicos <- data_final_pt %>%
  mutate(topico_lda = atribuicao_topico_por_musica$topico_mais_provavel)

# --- PASSO 3: Realizar as análises por grupo (tópico) ---

# Loop para analisar cada grupo (tópico)
num_topicos <- k # 'k' foi definido como 2 no seu modelo LDA

for (i in 1:num_topicos) {
  cat(paste0("\n=============================================\n"))
  cat(paste0("       ANÁLISE PARA O TÓPICO ", i, "\n"))
  cat(paste0("=============================================\n"))
  
  # Filtrar as músicas que pertencem a este tópico
  musicas_no_topico <- data_com_topicos %>%
    filter(topico_lda == i)
  
  # 1. Quantas músicas tem em cada grupo
  num_musicas <- nrow(musicas_no_topico)
  cat(paste0("\nTotal de músicas neste grupo: ", num_musicas, "\n"))
  
  # 2. Quais são as músicas em cada grupo (listar as 10 primeiras, por exemplo)
  cat("\nAlgumas músicas neste grupo:\n")
  print(head(musicas_no_topico$Nome.y, 10)) # Mostra as 10 primeiras músicas
  
  # 3. Quais são os artistas mais presentes em cada grupo
  cat("\nArtistas mais presentes neste grupo:\n")
  artistas_por_topico <- musicas_no_topico %>%
    group_by(Artista.y) %>%
    summarise(contagem = n()) %>%
    arrange(desc(contagem))
  
  print(head(artistas_por_topico, 10)) # Mostra os 10 artistas mais presentes
  
  # 4. Qual a média de ano por grupo
  media_ano <- mean(musicas_no_topico$Ano, na.rm = TRUE) # na.rm = TRUE para ignorar anos ausentes
  cat(paste0("\nMédia do ano das músicas neste grupo: ", round(media_ano), "\n"))
  
  cat("\n---------------------------------------------\n")
}

# Opcional: Se quiser ver o data.frame com os tópicos atribuídos
View(data_com_topicos)

#====================================================================================



#====================================================================================


# Certifique-se de que o pacote 'dplyr' está carregado
library(dplyr)

# Inicializar uma lista vazia para armazenar os artistas de cada tópico
lista_artistas_por_topico <- list()

# O seu loop original, mas com uma pequena modificação para coletar os dados
num_topicos <- k # 'k' é o número de tópicos do seu modelo LDA (neste caso, 2)

for (i in 1:num_topicos) {
  cat(paste0("\n=============================================\n"))
  cat(paste0("       ANÁLISE PARA O TÓPICO ", i, "\n"))
  cat(paste0("=============================================\n"))
  
  # Filtrar as músicas que pertencem a este tópico
  musicas_no_topico <- data_com_topicos %>%
    filter(topico_lda == i)
  
  # 1. Quantas músicas tem em cada grupo
  num_musicas <- nrow(musicas_no_topico)
  cat(paste0("\nTotal de músicas neste grupo: ", num_musicas, "\n"))
  
  # 2. Quais são as músicas em cada grupo (listar as 10 primeiras, por exemplo)
  cat("\nAlgumas músicas neste grupo:\n")
  print(head(musicas_no_topico$Nome.y, 10)) # Nomes das músicas
  
  # 3. Quais são os artistas mais presentes em cada grupo
  # (Esta parte será armazenada na lista, não impressa diretamente aqui)
  artistas_por_topico_atual <- musicas_no_topico %>%
    group_by(Artista.y) %>%
    summarise(contagem = n()) %>%
    arrange(desc(contagem)) %>%
    head(10) # Pegar os top 10 artistas para este tópico
  
  # Adicionar uma coluna para identificar o tópico
  artistas_por_topico_atual$Topico <- paste0("Tópico ", i)
  
  # Armazenar este data.frame na lista
  lista_artistas_por_topico[[i]] <- artistas_por_topico_atual
  
  # 4. Qual a média de ano por grupo
  media_ano <- mean(musicas_no_topico$Ano, na.rm = TRUE)
  cat(paste0("\nMédia do ano das músicas neste grupo: ", round(media_ano), "\n"))
  
  cat("\n---------------------------------------------\n")
}

# ===============================================
# TABELA CONSOLIDADA DOS ARTISTAS MAIS FREQUENTES
# ===============================================

# Combinar todos os data.frames de artistas da lista em um único data.frame
tabela_artistas_consolidados <- bind_rows(lista_artistas_por_topico)

cat("\n\n=============================================\n")
cat("  ARTISTAS MAIS FREQUENTES POR TÓPICO (CONSOLIDADO)\n")
cat("=============================================\n")
print(tabela_artistas_consolidados)

# Para uma visualização mais interativa (no RStudio)
# View(tabela_artistas_consolidados)

#===============================================================================================

library(dplyr)
library(tidyr) # Para a função pivot_wider()
library(tidyverse)

# Inicializar uma lista vazia para armazenar os artistas de cada tópico
lista_artistas_por_topico_pivot <- list()

for (i in 1:num_topicos) {
  # (As mensagens de progresso do loop para cada tópico podem continuar aqui se desejar)
  # cat(paste0("\n=============================================\n"))
  # cat(paste0("       ANÁLISE PARA O TÓPICO ", i, "\n"))
  # cat(paste0("=============================================\n"))
  # ... (resto do seu código de análise individual por tópico, omitido para focar na tabela)
  
  musicas_no_topico <- data_com_topicos %>%
    filter(topico_lda == i)
  
  artistas_por_topico_atual <- musicas_no_topico %>%
    group_by(Artista.y) %>%
    summarise(contagem = n()) %>%
    arrange(desc(contagem)) %>%
    head(10) %>% # Pegar os top 10 artistas para este tópico
    mutate(posicao = row_number()) %>% # Adicionar uma coluna de posição (1, 2, ..., 10)
    select(posicao, Artista.y) # Selecionar apenas a posição e o nome do artista
  
  # Renomear a coluna do artista para que seja o nome do tópico
  colnames(artistas_por_topico_atual)[colnames(artistas_por_topico_atual) == "Artista.y"] <- paste0("Tópico ", i)
  
  # Armazenar este data.frame na lista
  lista_artistas_por_topico_pivot[[i]] <- artistas_por_topico_atual
}

tabela_artistas_alinhada <- lista_artistas_por_topico_pivot %>%
  reduce(full_join, by = "posicao") %>% # 'reduce' e 'full_join' são do pacote purrr (parte do tidyverse)
  arrange(posicao) # Garantir que as posições estejam em ordem

cat("\n\n=============================================\n")
cat("    ARTISTAS MAIS FREQUENTES POR TÓPICO (ALINHADA)\n")
cat("=============================================\n")
print(tabela_artistas_alinhada)

View(tabela_artistas_alinhada)

#==================================================================================================================

# Certifique-se de que os pacotes necessários estão carregados
library(dplyr)
library(ggplot2) # Para a criação do gráfico

# --- PASSO 1: Preparar os dados para o gráfico ---

# Calcular o total de músicas por ano
total_musicas_por_ano <- data_com_topicos %>%
  group_by(Ano) %>%
  summarise(total_musicas = n())

# Calcular o número de músicas por tópico por ano
musicas_topico_ano <- data_com_topicos %>%
  group_by(Ano, topico_lda) %>%
  summarise(contagem_topico_ano = n()) %>%
  ungroup() # Remover o agrupamento temporário

# Juntar os dois data.frames para calcular a proporção
dados_grafico_proporcao <- musicas_topico_ano %>%
  left_join(total_musicas_por_ano, by = "Ano") %>%
  mutate(proporcao = contagem_topico_ano / total_musicas)

# Opcional: Inspecionar os dados preparados para o gráfico
# print(head(dados_grafico_proporcao))
# View(dados_grafico_proporcao)

# --- PASSO 2: Gerar o gráfico de linhas ---

cat("\n\n=============================================\n")
cat(" GRÁFICO: PROPORÇÃO DE MÚSICAS POR TÓPICO AO LONGO DO TEMPO\n")
cat("=============================================\n")

ggplot(dados_grafico_proporcao, aes(x = Ano, y = proporcao, color = factor(topico_lda))) +
  geom_line(linewidth = 1) + # linewidth = 1 para linhas um pouco mais grossas
  geom_point(size = 2) + # Pontos para cada ano
  labs(
    title = "Proporção de Músicas por Tópico ao Longo dos Anos",
    x = "Ano",
    y = "Proporção de Músicas",
    color = "Tópico LDA" # Legenda da cor
  ) +
  scale_y_continuous(labels = scales::percent) + # Formata o eixo Y como porcentagem
  theme_minimal() + # Um tema de gráfico limpo
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Centraliza e negrita o título
    legend.position = "right" # Posição da legenda
  )


cat("\n\n=============================================\n")
cat(" GRÁFICOS: PROPORÇÃO DE MÚSICAS POR TÓPICO (DE 2 EM 2)\n")
cat("=============================================\n")

# Loop para gerar gráficos de 2 em 2 tópicos
for (i in seq(1, 10, by = 2)) {
  # Definir os tópicos para o gráfico atual
  topicos_atuais <- c(i, i + 1)
  
  # Filtrar os dados para os tópicos atuais
  dados_subset <- filter(dados_grafico_proporcao, topico_lda %in% topicos_atuais)
  
  # Gerar o gráfico
  p <- ggplot(dados_subset, aes(x = Ano, y = proporcao, color = factor(topico_lda))) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = paste0("Proporção de Músicas por Tópico (K=", 10, ") - Tópicos ", i, " e ", i + 1),
      x = "Ano",
      y = "Proporção de Músicas",
      color = "Tópico LDA"
    ) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "right"
    )
  
  print(p) # Imprime cada gráfico individualmente
  cat(paste0("\nGráfico para Tópicos ", i, " e ", i + 1, " gerado.\n"))
}
