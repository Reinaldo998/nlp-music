library(dplyr)

# --- 1. Calcular o número médio e mediano de palavras (total e únicas) por música em cada tópico ---
# Verifica se 'dtm_filtrada' e 'data_dp' estão disponíveis
if (!exists("dtm_filtrada") || !exists("data_dp")) {
  stop("Os objetos 'dtm_filtrada' e/ou 'data_dp' não foram encontrados. Por favor, carregue-os.")
}

# Converte a DTM filtrada para uma matriz densa
dtm_matrix_dense <- as.matrix(dtm_filtrada)

# Calcula a soma das linhas da DTM original (total de ocorrências de palavras)
num_palavras_total_por_musica <- rowSums(dtm_matrix_dense)

# --- NOVO: Calcula a quantidade de palavras ÚNICAS por música ---
# 1. Cria uma matriz binária: qualquer valor > 0 se torna 1
dtm_matrix_binary <- (dtm_matrix_dense > 0) * 1 
# 2. Soma as linhas da matriz binária para obter o número de palavras únicas
num_palavras_unicas_por_musica <- rowSums(dtm_matrix_binary)

# Cria um dataframe temporário para mapear o Codigo da música ao número de palavras
df_num_palavras <- data.frame(
  Codigo = as.numeric(row.names(dtm_filtrada)), # Garante que Codigo é CHARACTER
  Num_Palavras_Total_DTM = num_palavras_total_por_musica, # Total de ocorrências
  Num_Palavras_Unicas_DTM = num_palavras_unicas_por_musica # NOVO: Quantidade de palavras únicas
)

# Junta este dataframe com a base principal (data_dp) para ter a informação do tópico
# A junção é feita pelo ID da música.
data_dp_com_palavras <- data_dp %>%
  left_join(df_num_palavras, by = c("ID" = "Codigo"))

# Calcula a média e a mediana de palavras por tópico
media_palavras_por_topico <- data_dp_com_palavras %>%
  group_by(Grupos_MM_DP) %>% # Agrupa as músicas pelo seu grupo (tópico) final
  summarise(
    Media_Palavras_Total_por_Musica = round(mean(Num_Palavras_Total_DTM, na.rm = TRUE), 2), # Média total
    Mediana_Palavras_Total_por_Musica = round(median(Num_Palavras_Total_DTM, na.rm = TRUE), 2), # Mediana total
    Media_Palavras_Unicas_por_Musica = round(mean(Num_Palavras_Unicas_DTM, na.rm = TRUE), 2), # NOVO: Média de palavras únicas
    Mediana_Palavras_Unicas_por_Musica = round(median(Num_Palavras_Unicas_DTM, na.rm = TRUE), 2), # NOVO: Mediana de palavras únicas
    .groups = "drop" # Remove o agrupamento após o summarise
  )

print("\n--- Número Médio e Mediano de Palavras (Total e Únicas) por Música em Cada Tópico ---")
print(media_palavras_por_topico)


resumo_palavras<-cbind(media_palavras_por_topico[,1],resumo_topicos_estudo[,2],media_palavras_por_topico[,2:5],resumo_topicos_estudo[,3])
resumo_palavras<-resumo_palavras%>%arrange(desc(num_musicas))

xtable(resumo_palavras)

View(resumo_palavras%>%arrange(desc(num_musicas)))


# --- 2. Encontrar as 10 palavras mais frequentes em cada grupo ---
library(dplyr)

# Certifique-se de que 'dtm_filtrada' e 'data_dp_com_palavras' estão disponíveis.

# --- 1. Calcular o número médio e mediano de palavras por letra em cada tópico ---
# ... (código anterior) ...

# --- 2. Encontrar as 10 palavras mais frequentes em cada grupo (CÓDIGO CORRIGIDO) ---

cat("\n--- Top 10 Palavras Mais Frequentes por Tópico ---\n")

termos_dtm <- colnames(dtm_filtrada)# trocar pra filtrada
topicos_existentes <- sort(unique(data_dp$Grupos_MM_DP))

# Inicialize um dataframe para armazenar os resultados
top_palavras_por_topico <- data.frame(
  Topico = integer(),
  Palavra = character(),
  Frequencia = integer(),
  stringsAsFactors = FALSE
)

# --- NOVO PASSO: Obter os Códigos válidos da DTM ---
codigos_validos_na_dtm <- row.names(dtm_filtrada)#trocar pra filtrada


# Loop sobre cada tópico para encontrar as top 10 palavras
for (topico_id in topicos_existentes) {
  # Obtenha os Códigos das músicas que pertencem ao tópico atual
  codigos_do_topico_nao_filtrado <- data_dp_com_palavras %>%
    filter(Grupos_MM_DP == topico_id) %>%
    pull(ID) %>%
    as.character()
  
  # Filtre os Códigos para ter certeza de que eles existem na DTM
  codigos_do_topico_filtrado <- codigos_do_topico_nao_filtrado[codigos_do_topico_nao_filtrado %in% codigos_validos_na_dtm]
  
  # Se houver Códigos válidos, continue
  if (length(codigos_do_topico_filtrado) > 0) {
    # Crie uma sub-matriz da DTM para o tópico, usando APENAS os Códigos válidos
    dtm_do_topico <- dtm_filtrada[codigos_do_topico_filtrado, ]
    
    # Se o DTM do tópico for apenas uma linha, transforme-o em uma matriz para evitar erros
    if (is.vector(dtm_do_topico)) {
      dtm_do_topico <- matrix(dtm_do_topico, nrow = 1, dimnames = list(codigos_do_topico_filtrado, names(dtm_do_topico)))
    }
    
    frequencia_termos_topico <- colSums(as.matrix(dtm_do_topico))
    
    top_10_termos <- head(sort(frequencia_termos_topico, decreasing = TRUE), 10)
    
    top_palavras_por_topico <- bind_rows(
      top_palavras_por_topico,
      data.frame(
        Topico = topico_id,
        Palavra = names(top_10_termos),
        Frequencia = as.numeric(top_10_termos)
      )
    )
  }
}

#View(top_palavras_por_topico)
print(top_palavras_por_topico)

#=============================================================================================================================

library(dplyr)
library(tidyr) # Para pivot_wider()
library(purrr) # Para reduce()

# --- 1. Identificar os 10 tópicos com mais músicas ---
# Esta parte é crucial para definir o escopo da análise.
# Assumimos que 'resumo_topicos_estudo' já existe.
if (!exists("resumo_topicos_estudo")) {
  stop("O objeto 'resumo_topicos_estudo' não foi encontrado. Por favor, carregue-o ou crie-o.")
}

top_10_topicos_por_musicas <- resumo_topicos_estudo %>%
  arrange(desc(num_musicas)) %>%
  head(10) %>%
  pull(Grupos_MM_DP) # Pega o ID do grupo

cat(paste0("--- Os 10 tópicos com mais músicas são: ", paste(top_10_topicos_por_musicas, collapse = ", "), " ---\n"))


# --- 2. Preparar e Pivotar a Tabela de Palavras ---

# Filtra a tabela de palavras para incluir apenas os 10 tópicos mais frequentes.
df_palavras_filtrado <- top_palavras_por_topico %>%
  filter(Topico %in% top_10_topicos_por_musicas) %>%
  group_by(Topico) %>%
  # Adiciona uma coluna de posição (de 1 a 10) para cada tópico
  mutate(posicao = row_number()) %>%
  ungroup() %>%
  select(-Frequencia) # Remove a coluna Frequencia, pois não será usada na tabela alinhada

# Pivotar a tabela do formato longo para o formato wide
tabela_top_palavras_alinhada <- df_palavras_filtrado %>%
  pivot_wider(
    names_from = Topico,
    values_from = Palavra,
    names_prefix = "Tópico "
  ) %>%
  arrange(posicao)

# Preencher NAs com strings vazias para uma exibição mais limpa
tabela_top_palavras_alinhada <- tabela_top_palavras_alinhada %>%
  mutate_all(~replace_na(., ""))

# --- 3. Visualizar a tabela alinhada ---
print("\n--- Tabela Alinhada com os Top 10 Termos por Tópico ---")
View(tabela_top_palavras_alinhada)
print(tabela_top_palavras_alinhada)

xtable(tabela_top_palavras_alinhada)

# Opcional: Salvar a tabela alinhada
# write.csv(tabela_top_palavras_alinhada, "top_10_palavras_por_topico.csv", row.names = FALSE, fileEncoding = "UTF-8")

# --- Carregar pacotes necessários (se ainda não estiverem) ---
library(dplyr) # Para manipulação de dataframes
library(tidyr) # Para pivot_longer (se necessário) ou para lidar com NA

# --- Suponha que 'dtm_filtrada' e 'tabela_top_palavras_alinhada' já estão carregadas na memória ---
# 'dtm_filtrada': Sua matriz DTM original (Documento x Termo).
# 'tabela_top_palavras_alinhada': O dataframe que contém as top 10 palavras para cada tópico.

# =================================================================================
# 1. Extrair todas as palavras únicas da tabela_top_palavras_alinhada
# =================================================================================

# Converte a tabela alinhada para o formato longo para extrair todas as palavras
# O select(-posicao) remove a coluna de posição, pois não precisamos dela para as palavras
# O filter(palavra != "") remove as células que foram preenchidas com string vazia (NA)
palavras_top10_unicas <- tabela_top_palavras_alinhada %>%
  pivot_longer(
    cols = -posicao, # Seleciona todas as colunas exceto 'posicao'
    names_to = "Topico",
    values_to = "palavra"
  ) %>%
  filter(palavra != "") %>% # Remove strings vazias (que eram NAs)
  pull(palavra) %>%
  unique() # Garante que cada palavra apareça apenas uma vez na lista

cat("Total de palavras únicas encontradas nas top 10 listas:", length(palavras_top10_unicas), "\n")

# =================================================================================
# 2. Filtrar a matriz 'dtm_filtrada' para manter apenas as colunas dessas palavras
# =================================================================================

# Identifica quais das palavras top 10 realmente existem nas colunas da dtm_filtrada
palavras_existentes_na_dtm <- colnames(dtm_filtrada)[colnames(dtm_filtrada) %in% palavras_top10_unicas]

if (length(palavras_existentes_na_dtm) == 0) {
  stop("Nenhuma das palavras top 10 encontradas na tabela existe nas colunas da dtm_filtrada. 
        Verifique a consistência dos nomes das palavras.")
}

# Cria a nova matriz DTM filtrada
dtm_filtrada_palavras_top10 <- dtm_filtrada[, palavras_existentes_na_dtm]

cat("Nova matriz 'dtm_filtrada_palavras_top10' criada com", ncol(dtm_filtrada_palavras_top10), "colunas (palavras).\n")
cat("Dimensões da nova DTM:", dim(dtm_filtrada_palavras_top10)[1], "linhas (músicas) x", dim(dtm_filtrada_palavras_top10)[2], "colunas (palavras).\n")
dim(dtm_filtrada_palavras_top10)
# =================================================================================
# 3. Resultado
# =================================================================================
# A nova matriz DTM resultante é 'dtm_filtrada_palavras_top10'.
# Você pode inspecioná-la com:
# head(dtm_filtrada_palavras_top10[, 1:min(10, ncol(dtm_filtrada_palavras_top10))]) # Primeiras linhas e colunas
#dim(dtm_filtrada_palavras_top10) # Dimensões
