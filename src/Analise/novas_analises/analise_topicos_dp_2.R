# --- NOVO PASSO: Criar o resumo dos tópicos (resumo_topicos_estudo) ---
# Isso é o que faltava para definir 'resumo_topicos_estudo'.
resumo_topicos_estudo <- data_dp %>%
  group_by(Grupos_MM_DP) %>% # <--- CORRIGIDO: Usar o nome da coluna correto
  summarise(
    num_musicas = n(),
    media_ano = round(mean(Ano, na.rm = TRUE)) # Calculando a média do ano também, se desejar
  ) %>%
  ungroup() %>%
  arrange(Grupos_MM_DP) # <--- CORRIGIDO: Usar o nome da coluna correto

print("Resumo de tópicos (resumo_topicos_estudo) criado:")
View(resumo_topicos_estudo)
print(dim(resumo_topicos_estudo))

# --- 1. Identificar os 10 tópicos com mais músicas ---
# Agora 'resumo_topicos_estudo' está disponível.
top_10_topicos_por_musicas <- resumo_topicos_estudo %>%
  arrange(desc(num_musicas)) %>% # Ordena os tópicos pela contagem de músicas
  head(10) %>% # Pega os 10 primeiros
  pull(Grupos_MM_DP) # <--- CORRIGIDO: Usar o nome da coluna correto

cat(paste0("--- Os 10 tópicos com mais músicas são: ", paste(top_10_topicos_por_musicas, collapse = ", "), " ---\n"))
#===================================================================================

# Certifique-se de que as variáveis 'data_dp' e 'top_10_topicos_por_musicas'top10 artistas
# já estão na sua sessão do R e com os dados corretos.

# Inicializar uma lista vazia para armazenar os data.frames de artistas de cada um desses tópicos
lista_artistas_top_10_musicas <- list()

# --- 2. Iterar APENAS sobre os 10 tópicos selecionados ---
# Este loop cria um dataframe de top artistas para cada um dos 10 tópicos
for (i in top_10_topicos_por_musicas) {
  # Filtrar as músicas que pertencem ao tópico atual
  musicas_no_topico_dp <- data_dp %>%
    filter(Grupos_MM_DP == i)
  
  # Calcular os top 10 artistas para este tópico
  artistas_por_topico_corrente_dp <- musicas_no_topico_dp %>%
    group_by(Artista.y) %>% # Assumindo 'Artista.y' é a coluna do nome do artista
    summarise(contagem = n()) %>%
    arrange(desc(contagem)) %>%
    head(10) %>% # Pegar os top 10 artistas
    mutate(posicao = row_number()) %>% # Adicionar uma coluna de posição (1, 2, ..., 10)
    select(posicao, Artista.y) # Selecionar apenas a posição e o nome do artista
  
  # Renomear a coluna do artista para que seja o nome do tópico
  colnames(artistas_por_topico_corrente_dp)[colnames(artistas_por_topico_corrente_dp) == "Artista.y"] <- paste0("Tópico ", i)
  
  # Armazenar este data.frame na lista
  lista_artistas_top_10_musicas[[as.character(i)]] <- artistas_por_topico_corrente_dp
}

# --- 3. Consolidar em uma única tabela alinhada ---
cat("\n\n=============================================\n")
cat(" ARTISTAS MAIS FREQUENTES NOS 10 TÓPICOS COM MAIS MÚSICAS (ALINHADA)\n")
cat("=============================================\n")

if (length(lista_artistas_top_10_musicas) > 0) {
  tabela_artistas_alinhada_top_musicas <- lista_artistas_top_10_musicas %>%
    purrr::reduce(full_join, by = "posicao") %>%
    arrange(posicao) %>%
    mutate_all(~replace_na(., "")) # Preencher NAs com strings vazias para uma exibição mais limpa
  
  View(tabela_artistas_alinhada_top_musicas)
  print(tabela_artistas_alinhada_top_musicas)
} else {
  cat("Não foram encontrados tópicos suficientes ou dados para gerar a tabela alinhada.\n")
}

#library(xtable)

#xtable(tabela_artistas_alinhada_top_musicas)
#==============================================================================================


# --- 1. Preparar os dados para os gráficos de proporção por ano (APENAS os 10 tópicos mais frequentes) ---

dados_filtrados_para_grafico <- data_dp %>%
  filter(Grupos_MM_DP %in% top_10_topicos_por_musicas)

total_musicas_por_ano_dp <- data_dp %>%
  group_by(Ano) %>%
  summarise(total_musicas = n(), .groups = "drop")

musicas_topico_ano_dp_filtrado <- dados_filtrados_para_grafico %>%
  group_by(Ano, Grupos_MM_DP) %>%
  summarise(contagem_topico_ano = n(), .groups = "drop")

dados_grafico_proporcao_dp <- musicas_topico_ano_dp_filtrado %>%
  left_join(total_musicas_por_ano_dp, by = "Ano") %>%
  mutate(proporcao = contagem_topico_ano / total_musicas)

print("Dados para o gráfico preparados com sucesso.")

# --- 2. Gerar o gráfico estático ggplot2 ---

cat("\n\n=============================================\n")
cat(" GRÁFICO ESTÁTICO: PROPORÇÃO DOS 10 TÓPICOS MAIS FREQUENTES\n")
cat("=============================================\n")

# A correção é aqui, simplificando o 'aes' para isolar o problema.
# O 'group' é crucial para geom_line.
p_estatico <- ggplot(dados_grafico_proporcao_dp,
                     aes(x = Ano, y = proporcao)) +
  geom_line(aes(color = factor(Grupos_MM_DP), group = factor(Grupos_MM_DP)), linewidth = 1) +
  geom_point(aes(color = factor(Grupos_MM_DP)), size = 2) +
  labs(
    title = "Proporção dos 10 Tópicos Mais Frequentes (Data_DP) ao Longo dos Anos",
    x = "Ano",
    y = "Proporção de Músicas",
    color = "Tópico"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

print(p_estatico)