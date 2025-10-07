# --- 0. Carregar TODOS os Pacotes Necessários (no início do seu script) ---
library(topicmodels) # Para LDA
library(dplyr)       # Para manipulação de dados
library(tidyr)       # Para pivot_wider
library(purrr)       # Para reduce (usado com full_join)
library(ggplot2)     # Para a criação dos gráficos
library(scales)      # Para scale_y_continuous(labels = scales::percent)
# library(stringr)   # Se ainda precisar para alguma limpeza, mas parece não ser mais usada diretamente nos blocos abaixo

# --- 1. Carregar a DTM Filtrada e os Dados Originais ---
# Certifique-se de que dtm_filtrada e data_final_pt estão disponíveis.
# Se você salvou, carregue-os:
dtm_filtrada <- readRDS("dtm_filtrada.rds")
# data_final_pt <- readRDS("seu_data_final_pt.rds") # Exemplo: carregue seu data_final_pt original aqui

min(colSums(as.matrix(dtm_filtrada)))

# --- 2. Definir o Número de Tópicos para esta Análise ---
k_atual <- 10 # <--- DEFINE AQUI O NÚMERO DE TÓPICOS QUE VOCÊ DESEJA PARA ESTA EXECUÇÃO!

# --- 3. Ajustar o Modelo LDA ---
ap_lda_atual <- LDA(dtm_filtrada, k = k_atual, method = "Gibbs")

# --- 4. Obter Atribuições de Tópicos e Combinar com os Dados Originais ---
prob_documento_topico_atual <- posterior(ap_lda_atual)$topics
atribuicao_topico_atual <- as.data.frame(prob_documento_topico_atual) %>%
  mutate(topico_mais_provavel = max.col(.)) %>%
  select(topico_mais_provavel)

data_com_topicos_atual <- data_final_pt %>%
  mutate(topico_lda = atribuicao_topico_atual$topico_mais_provavel)


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


# --- 7. Preparar Dados para Gráficos de Proporção por Ano ---
total_musicas_por_ano_graf <- data_com_topicos_atual %>%
  group_by(Ano) %>%
  summarise(total_musicas = n())

musicas_topico_ano_graf <- data_com_topicos_atual %>%
  group_by(Ano, topico_lda) %>%
  summarise(contagem_topico_ano = n()) %>%
  ungroup()

dados_grafico_proporcao_atual <- musicas_topico_ano_graf %>%
  left_join(total_musicas_por_ano_graf, by = "Ano") %>%
  mutate(proporcao = contagem_topico_ano / total_musicas)


# --- 8. Gerar o Gráfico de Proporção de Músicas por Tópico (TODAS as linhas juntas) ---
cat("\n\n=============================================\n")
cat(" GRÁFICO: PROPORÇÃO DE MÚSICAS POR TÓPICO AO LONGO DO TEMPO (TODOS OS TÓPICOS)\n")
cat("=============================================\n")

ggplot(dados_grafico_proporcao_atual, aes(x = Ano, y = proporcao, color = factor(topico_lda))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = paste0("Proporção de Músicas por Tópico (K=", k_atual, ") - Todos os Tópicos"),
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

# --- 9. Gerar os Gráficos de Proporção de Músicas por Tópico (de 2 em 2) ---
cat("\n\n=============================================\n")
cat(paste0(" GRÁFICOS: PROPORÇÃO DE MÚSICAS POR TÓPICO (K = ", k_atual, ", DE 2 EM 2)\n"))
cat("=============================================\n")

for (i in seq(1, k_atual, by = 2)) {
  topicos_para_plot <- c(i, i + 1)
  
  # Lida com o caso de um número ímpar de tópicos no último gráfico
  if (k_atual %% 2 != 0 && i == k_atual) { # Se k_atual é ímpar e estamos no último tópico
    topicos_para_plot <- c(i) # Plota apenas o último tópico
    titulo_topicos <- paste0("Tópico ", i)
  } else {
    titulo_topicos <- paste0("Tópicos ", i, " e ", i + 1)
  }
  
  dados_subset_graf <- filter(dados_grafico_proporcao_atual, topico_lda %in% topicos_para_plot)
  
  p_graf <- ggplot(dados_subset_graf, aes(x = Ano, y = proporcao, color = factor(topico_lda))) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = paste0("Proporção de Músicas por Tópico (K=", k_atual, ") - ", titulo_topicos),
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
  
  print(p_graf) # Imprime cada gráfico individualmente
  cat(paste0("\nGráfico para ", titulo_topicos, " gerado.\n"))
}

#=========================================================================================

# --- 0. Certifique-se de que o pacote 'plotly' está carregado ---
# library(plotly) # Já deve estar carregado no seu script completo.

# --- 1. Use os dados já preparados ---
# O 'dados_grafico_proporcao_atual' já deve estar disponível em sua sessão,
# vindo da seção "7. Preparar Dados para Gráficos de Proporção por Ano" do seu código completo.
# A variável 'k_atual' também deve estar definida.

# --- 2. Gerar o gráfico ggplot2 base com estética 'text' para o balão ---
p_lda_todos_topicos_interativo <- ggplot(dados_grafico_proporcao_atual,
                                         aes(x = Ano, y = proporcao,
                                             color = factor(topico_lda), # Cor diferente para cada tópico
                                             group = factor(topico_lda), # Agrupamento explícito
                                             # Informações para o balão interativo (tooltip)
                                             text = paste("Tópico: ", topico_lda, # Usamos topico_lda pois é o nome da coluna aqui
                                                          "<br>Ano: ", Ano,
                                                          "<br>Proporção: ", scales::percent(proporcao)))) +
  geom_line(linewidth = 1) +  # Linhas
  geom_point(size = 2) +      # E pontos
  labs(
    title = paste0("Proporção de Músicas por Tópico (LDA, K=", k_atual, ") - Todos os Tópicos"),
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

# --- 3. Converter para Plotly ---
# Passamos o objeto ggplot diretamente para ggplotly
plotly_lda_todos_topicos_final <- ggplotly(p_lda_todos_topicos_interativo, tooltip = "text")

# --- 4. Exibir o gráfico interativo ---
cat("\n\n=============================================\n")
cat(paste0(" GRÁFICO INTERATIVO: PROPORÇÃO DE MÚSICAS POR TÓPICO AO LONGO DO TEMPO (TODOS OS TÓPICOS LDA)\n"))
cat("=============================================\n")

print(plotly_lda_todos_topicos_final)

# Opcional: Salvar o gráfico interativo em HTML
library(htmlwidgets) # Carregue se for salvar
caminho_arquivo_html_lda_todos <- paste0("grafico_lda_k", k_atual, "_todos_topicos_interativo.html")
saveWidget(plotly_lda_todos_topicos_final, file = caminho_arquivo_html_lda_todos, selfcontained = TRUE)
cat(paste0("\nGráfico interativo salvo como: '", caminho_arquivo_html_lda_todos, "'\n"))

library(xtable)
xtable(tabela_artistas_alinhada_atual)
