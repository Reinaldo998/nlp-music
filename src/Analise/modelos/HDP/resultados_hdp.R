# Carregar o DataFrame principal com os tópicos HDP
musicas_com_topicos_hdp <- read_csv("Reinaldo/HDP/musicas_com_topico.csv")
head(musicas_com_topicos_hdp)
str(musicas_com_topicos_hdp)

# Carregar os top termos por tópico
#hdp_top_termos <- read_csv("Reinaldo/HDP/hdp_top_termos.csv")
View(hdp_top_termos)
# Carregar o resumo de músicas por tópico
hdp_resumo_topicos <- read_csv("Reinaldo/HDP/hdp_resumo_topicos.csv")


# Carregar os top artistas por tópico
hdp_top_artistas_por_topico <- read_csv("Reinaldo/HDP/hdp_top_artistas_por_topico.csv")

#=============================================================================================

# Certifique-se de que os pacotes necessários estão carregados
library(dplyr)
library(tidyr) # Para pivot_wider()
library(purrr) # Para reduce()
library(readr) # Para read_csv, se ainda não estiver carregado

# --- 1. Carregar os Dados do HDP (Conforme seu código) ---
# Se você já rodou estas linhas e os objetos estão na memória, pode pular.
#musicas_com_topicos_hdp <- read_csv("Reinaldo/HDP/musicas_com_topicos_hdp.csv")
# hdp_top_termos <- read_csv("Reinaldo/HDP/hdp_top_termos.csv") # Não usado diretamente neste bloco
#hdp_resumo_topicos <- read_csv("Reinaldo/HDP/hdp_resumo_topicos.csv")
# hdp_top_artistas_por_topico <- read_csv("Reinaldo/HDP/hdp_top_artistas_por_topico.csv") # Esta é a saída que queremos gerar

# --- 2. Identificar os 10 tópicos HDP com mais músicas ---
# A coluna com a contagem de músicas em hdp_resumo_topicos é 'num_musicas',
# e a coluna com o ID do tópico é 'topico_hdp'.
top_10_topicos_hdp_por_musicas <- hdp_resumo_topicos %>%
  arrange(desc(num_musicas)) %>% # Ordena pela contagem de músicas (maior para menor)
  head(10) %>% # Pega os 10 primeiros tópicos
  pull(topico_hdp) # Extrai apenas os números dos tópicos

cat(paste0("--- Os 10 tópicos HDP com mais músicas são: ", paste(top_10_topicos_hdp_por_musicas, collapse = ", "), " ---\n"))

# Inicializar uma lista vazia para armazenar os data.frames de artistas de cada um desses tópicos
lista_artistas_hdp_top_10_musicas <- list()

# --- 3. Iterar APENAS sobre os 10 tópicos HDP selecionados ---
for (i in top_10_topicos_hdp_por_musicas) {
  # Filtrar as músicas que pertencem ao tópico HDP atual
  # A coluna de tópicos no 'musicas_com_topicos_hdp' é 'topico_hdp'.
  musicas_no_topico_hdp_atual <- musicas_com_topicos_hdp %>%
    filter(topico_hdp == i)
  
  # Calcular os top 10 artistas para este tópico HDP
  # A coluna do nome do artista no seu df é 'Artista.y'
  artistas_por_topico_hdp_corrente <- musicas_no_topico_hdp_atual %>%
    group_by(Artista.y) %>%
    summarise(contagem = n()) %>%
    arrange(desc(contagem)) %>%
    head(10) %>% # Pegar os top 10 artistas
    mutate(posicao = row_number()) %>% # Adicionar uma coluna de posição (1, 2, ..., 10)
    select(posicao, Artista.y) # Selecionar apenas a posição e o nome do artista
  
  # Renomear a coluna do artista para que seja o nome do tópico HDP
  colnames(artistas_por_topico_hdp_corrente)[colnames(artistas_por_topico_hdp_corrente) == "Artista.y"] <- paste0("Tópico HDP ", i)
  
  # Armazenar este data.frame na lista
  lista_artistas_hdp_top_10_musicas[[as.character(i)]] <- artistas_por_topico_hdp_corrente
}

# --- 4. Consolidar em uma única tabela alinhada para HDP ---
cat("\n\n=============================================\n")
cat(" ARTISTAS MAIS FREQUENTES NOS 10 TÓPICOS HDP COM MAIS MÚSICAS (ALINHADA)\n")
cat("=============================================\n")

if (length(lista_artistas_hdp_top_10_musicas) > 0) {
  tabela_artistas_hdp_alinhada_top_musicas <- lista_artistas_hdp_top_10_musicas %>%
    reduce(full_join, by = "posicao") %>%
    arrange(posicao) %>%
    mutate_all(~replace_na(., "")) # Preencher NAs com strings vazias para uma exibição mais limpa
  
  print(tabela_artistas_hdp_alinhada_top_musicas)
  
  # Opcional: Salvar esta tabela alinhada para uso futuro
  # saveRDS(tabela_artistas_hdp_alinhada_top_musicas, file = "hdp_artistas_alinhados_top_10_musicas.rds")
  # write.csv(tabela_artistas_hdp_alinhada_top_musicas, file = "hdp_artistas_alinhados_top_10_musicas.csv", row.names = FALSE)
  
} else {
  cat("Não foram encontrados tópicos HDP suficientes ou dados para gerar a tabela alinhada.\n")
}

xtable(tabela_artistas_hdp_alinhada_top_musicas)

#=================================================================================================================================

library(ggplot2)
library(scales)  # Para formatar o eixo Y como porcentagem
library(plotly)  # Para tornar o gráfico interativo

cat(paste0("--- Gerando gráfico para os 10 tópicos HDP com mais músicas: ", paste(top_10_topicos_hdp_por_musicas, collapse = ", "), " ---\n"))

# --- 3. Preparar os dados para o gráfico de proporção por ano (APENAS os 10 tópicos mais frequentes) ---

# Filtrar o DataFrame principal para incluir apenas os dados desses 10 tópicos
dados_filtrados_para_grafico <- musicas_com_topicos_hdp %>%
  filter(topico_hdp %in% top_10_topicos_hdp_por_musicas)

# Calcular o total de músicas por ano (em relação ao dataset COMPLETO para proporção correta)
total_musicas_por_ano_completo <- musicas_com_topicos_hdp %>%
  group_by(Ano) %>%
  summarise(total_musicas = n())

# Calcular o número de músicas por tópico por ano, apenas para os 10 tópicos filtrados
musicas_topico_ano_filtrado <- dados_filtrados_para_grafico %>%
  group_by(Ano, topico_hdp) %>%
  summarise(contagem_topico_ano = n()) %>%
  ungroup()

# Juntar para calcular a proporção (contagem do tópico filtrado / total de músicas no ano)
dados_grafico_proporcao_filtrado <- musicas_topico_ano_filtrado %>%
  left_join(total_musicas_por_ano_completo, by = "Ano") %>%
  mutate(proporcao = contagem_topico_ano / total_musicas)

# --- 4. Gerar o gráfico de linhas no ggplot2 (APENAS as linhas para os 10 tópicos mais frequentes) ---

cat("\n\n=============================================\n")
cat(" GRÁFICO GGPLOT2: PROPORÇÃO DOS 10 TÓPICOS HDP MAIS FREQUENTES POR ANO (APENAS LINHAS)\n")
cat("=============================================\n")

ggplot(dados_grafico_proporcao_filtrado,
       aes(x = Ano, y = proporcao, color = factor(topico_hdp))) +
  geom_line(linewidth = 1) + # APENAS LINHAS
  # geom_point(size = 2) + # REMOVIDO os pontos
  labs(
    title = "Proporção dos 10 Tópicos HDP Mais Frequentes ao Longo dos Anos",
    x = "Ano",
    y = "Proporção de Músicas",
    color = "Tópico HDP"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

#=========================================================================================================

cat(paste0("--- Gerando gráfico interativo para os 10 tópicos HDP com mais músicas: ", paste(top_10_topicos_hdp_por_musicas, collapse = ", "), " ---\n"))

# --- 3. Preparar os dados para o gráfico de proporção por ano (APENAS os 10 tópicos mais frequentes) ---
dados_filtrados_para_grafico <- musicas_com_topicos_hdp %>%
  filter(topico_hdp %in% top_10_topicos_hdp_por_musicas)

total_musicas_por_ano_completo <- musicas_com_topicos_hdp %>%
  group_by(Ano) %>%
  summarise(total_musicas = n())

musicas_topico_ano_filtrado <- dados_filtrados_para_grafico %>%
  group_by(Ano, topico_hdp) %>%
  summarise(contagem_topico_ano = n()) %>%
  ungroup()

dados_grafico_proporcao_filtrado <- musicas_topico_ano_filtrado %>%
  left_join(total_musicas_por_ano_completo, by = "Ano") %>%
  mutate(proporcao = contagem_topico_ano / total_musicas)

# --- 4. Gerar o gráfico ggplot2 base (linhas E pontos) ---
# O 'text' aesthetic é fundamental para o balão interativo do Plotly
p_base_ploty <- ggplot(dados_grafico_proporcao_filtrado,
                       aes(x = Ano, y = proporcao, color = factor(topico_hdp),
                           # Informações para o balão interativo (tooltip)
                           text = paste("Tópico: ", topico_hdp,
                                        "<br>Ano: ", Ano,
                                        "<br>Proporção: ", scales::percent(proporcao)))) +
  geom_line(linewidth = 1) +  # Linhas
  geom_point(size = 2) +      # E pontos
  labs(
    title = "Proporção dos 10 Tópicos HDP Mais Frequentes ao Longo dos Anos",
    x = "Ano",
    y = "Proporção de Músicas",
    color = "Tópico HDP"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

# --- 5. Converter para Plotly ---
# Passamos o objeto ggplot diretamente para ggplotly
# O argumento 'tooltip = "text"' diz ao Plotly para usar a estética 'text' que definimos no ggplot2 para o balão
plotly_hdp_top10_proporcao_final <- ggplotly(p_base_ploty, tooltip = "text")

# --- 6. Exibir o gráfico interativo ---
cat("\n\n=============================================\n")
cat(" GRÁFICO INTERATIVO: PROPORÇÃO DOS 10 TÓPICOS HDP MAIS FREQUENTES POR ANO (PLOTLY COM LINHAS E PONTOS)\n")
cat("=============================================\n")

print(plotly_hdp_top10_proporcao_final)

#==============================================================================================================

cat(paste0("--- Gerando gráfico interativo para os 10 tópicos HDP com mais músicas: ", paste(top_10_topicos_hdp_por_musicas, collapse = ", "), " ---\n"))

# --- 3. Preparar os dados para o gráfico de proporção por ano ---
dados_filtrados_para_grafico <- musicas_com_topicos_hdp %>%
  filter(topico_hdp %in% top_10_topicos_hdp_por_musicas)

total_musicas_por_ano_completo <- musicas_com_topicos_hdp %>%
  group_by(Ano) %>%
  summarise(total_musicas = n())

musicas_topico_ano_filtrado <- dados_filtrados_para_grafico %>%
  group_by(Ano, topico_hdp) %>%
  summarise(contagem_topico_ano = n()) %>%
  ungroup()

dados_grafico_proporcao_filtrado <- musicas_topico_ano_filtrado %>%
  left_join(total_musicas_por_ano_completo, by = "Ano") %>%
  mutate(proporcao = contagem_topico_ano / total_musicas)

# --- 4. Gerar o gráfico ggplot2 base (linhas E pontos) e converter para plotly ---

cat("\n\n=============================================\n")
cat(" GRÁFICO INTERATIVO: PROPORÇÃO DOS 10 TÓPICOS HDP MAIS FREQUENTES POR ANO (PLOTLY - LINHAS E PONTOS)\n")
cat("=============================================\n")

# Crie o objeto ggplot base
p_final_plotly <- ggplot(dados_grafico_proporcao_filtrado,
                         aes(x = Ano, y = proporcao,
                             color = factor(topico_hdp), # Cor diferente para cada tópico
                             group = factor(topico_hdp), # <--- ADICIONAMOS O AGRUPAMENTO AQUI!
                             # Informações para o balão interativo (tooltip)
                             text = paste("Tópico: ", topico_hdp,
                                          "<br>Ano: ", Ano,
                                          "<br>Proporção: ", scales::percent(proporcao)))) +
  geom_line(linewidth = 1) +  # Linhas
  geom_point(size = 2) +      # E pontos
  labs(
    title = "Proporção dos 10 Tópicos HDP Mais Frequentes ao Longo dos Anos HDP",
    x = "Ano",
    y = "Proporção de Músicas",
    color = "Tópico HDP"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

# Converter para plotly
plotly_hdp_top10_proporcao_final <- ggplotly(p_final_plotly, tooltip = "text")

# Exibir o gráfico interativo
print(plotly_hdp_top10_proporcao_final)

library(htmlwidgets) # Necessário para salvar o gráfico Plotly em HTML

# --- Salvar o Gráfico Interativo ---
# O objeto do seu gráfico interativo é 'plotly_hdp_top10_proporcao_final'

# Defina o caminho e nome do arquivo de saída
# Ele será salvo no seu diretório de trabalho atual, ou em um caminho específico
caminho_arquivo_html <- "grafico_hdp_top10_proporcao_interativo.html"

# Use a função saveWidget() do pacote htmlwidgets para salvar
saveWidget(plotly_hdp_top10_proporcao_final, file = caminho_arquivo_html, selfcontained = TRUE)

cat(paste0("\nGráfico interativo salvo como: '", caminho_arquivo_html, "'\n"))
cat("Você pode abrir este arquivo HTML em qualquer navegador da web para ver o gráfico interativo.\n")
