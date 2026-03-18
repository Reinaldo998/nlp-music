library(dplyr)

# Certifique-se de que os seguintes dataframes estão carregados em seu ambiente R:
# - data_final_pt (seu dataframe original com '...1' e 'Idioma')
# - df_final_musicas_iguais_artistas_diferentes (do passo de categorização final)
# - df_final_musicas_e_artistas_iguais (do passo de categorização final)
# - df_final_dados_web_nao_encontrados (o dataframe de NAs, do passo de categorização final)
# - df_final_musicas_e_artistas_diferentes (o dataframe de Músicas Diferentes, Artistas Diferentes do passo de categorização final)
# - df_consolidado_limpo (o dataframe intermediário com as colunas _limpo_final e dados Nome.y/Artista.y/Letra do site)


# --- FUNÇÃO DE LIMPEZA ABRANGENTE PARA COMPARAÇÃO ---
# Esta função será usada para limpar strings antes de compará-las para a condição de "retorno".
clean_string_for_comparison <- function(text) {
  if (is.character(text)) {
    # 1. Converter para minúsculas e remover espaços extras no início/fim
    text <- tolower(trimws(text))
    # 2. Remover texto entre parênteses (ex: (ao vivo), (remix))
    text <- gsub("\\s*\\([^\\)]*\\)", "", text)
    # 3. Remover caracteres não alfanuméricos e não espaços (inclui pontuação e alguns símbolos)
    text <- gsub("[^[:alnum:]\\s]", "", text)
    # 4. Remover dígitos (números)
    text <- gsub("[[:digit:]]", "", text)
    # 5. Remover acentos
    text <- chartr(
      "áéíóúÁÉÍÓÚàèìòùÀÈÌÒÙãõñÃÕÑâêîôûÂÊÎÔÛçÇ",
      "aeiouAEIOUaeiouAEIOUaonAONaeiouAEIOUcC",
      text
    )
    # 6. Remover espaços múltiplos e trim final novamente
    text <- gsub("\\s+", " ", text) # Reduz múltiplos espaços para um único
    text <- trimws(text)
  }
  return(text)
}


# --- 1. Identificar IDs das músicas que INICIALMENTE seriam retiradas ---
ids_inicialmente_para_retirar <- unique(c(
  df_final_dados_web_nao_encontrados$Codigo,
  df_final_musicas_e_artistas_diferentes$Codigo
))

# Inspecionar IDs inicialmente para retirar (opcional)
print("IDs que seriam inicialmente retirados do dataframe principal:")
print(ids_inicialmente_para_retirar)


# --- 2. Criar o dataframe df_musicas_retiradas_detalhado (com originais e raspados) ---
# Filtrar o dataframe ORIGINAL (data_final_pt) pelas músicas a serem inicialmente retiradas
df_base_para_retiradas <- data_final_pt %>%
  filter(ID %in% ids_inicialmente_para_retirar)

# Juntar este subconjunto com o df_consolidado_limpo para adicionar os valores raspados
df_musicas_retiradas_detalhado <- df_base_para_retiradas %>%
  left_join(
    df_consolidado_limpo %>%
      select(Codigo, Nome.y_raspado = Nome.y, Artista.y_raspado = Artista.y, Letra_raspada = Letra),
    by = c("ID" = "Codigo") # Junção pelo ID
  ) %>%
  select(
    `...1`, # Manter a coluna ...1 para o dataframe de retiradas, se desejado
    Codigo = ID, # O ID original, renomeado para Codigo
    Nome_Original = Nome.x,
    Artista_Original = Artista.x,
    Ano,
    Nome_Original_Y = Nome.y,       # Nome.y original do data_final_pt (antes da raspagem)
    Artista_Original_Y = Artista.y, # Artista.y original do data_final_pt (antes da raspagem)
    Letra_Original = Letra,         # Letra original do data_final_pt (antes da raspagem)
    Nome_Raspado = Nome.y_raspado,     # Nome da música raspado do site
    Artista_Raspado = Artista.y_raspado, # Artista raspado do site
    Letra_Raspada = Letra_raspada,      # Letra raspada do site
    Idioma # Manter a coluna Idioma para o dataframe de retiradas, se desejado
  )

# Inspecionar o dataframe de músicas retiradas detalhado (opcional)
print("Dataframe - Músicas Retiradas (Com valores Originais e Raspados, antes da verificação final):")
View(df_musicas_retiradas_detalhado)
print(dim(df_musicas_retiradas_detalhado))


# --- NOVO PASSO: Verificação e Ajuste Final do df_musicas_retiradas_detalhado ---

# 2.1. Identificar IDs das músicas que devem "voltar" para a base principal
# Aplicar a limpeza abrangente antes da comparação
ids_para_retornar_a_base <- df_musicas_retiradas_detalhado %>%
  # Adicionar colunas temporárias limpas para a comparação
  mutate(
    Nome_Original_Comp = sapply(Nome_Original, clean_string_for_comparison),
    Nome_Original_Y_Comp = sapply(Nome_Original_Y, clean_string_for_comparison)
  ) %>%
  # Filtrar pela condição de retorno: Nome_Original limpo é igual a Nome_Original_Y limpo
  filter(
    !is.na(Nome_Original_Comp), # Garantir que não estamos comparando NAs
    !is.na(Nome_Original_Y_Comp),
    Nome_Original_Comp == Nome_Original_Y_Comp
  ) %>%
  pull(Codigo) # Extrai apenas o vetor de Codigos

# Inspecionar IDs para retornar (opcional)
print("IDs de músicas que retornarão para a base principal de análise (após limpeza):")
print(ids_para_retornar_a_base)

# 2.2. Ajustar df_musicas_retiradas_detalhado: remover as músicas que estão "voltando"
df_musicas_retiradas_detalhado_final <- df_musicas_retiradas_detalhado %>%
  filter(!(Codigo %in% ids_para_retornar_a_base))

# Inspecionar o dataframe final de músicas retiradas (opcional)
print("Dataframe - Músicas Retiradas (FINAL - após verificação e retorno):")
View(df_musicas_retiradas_detalhado_final)
print(dim(df_musicas_retiradas_detalhado_final))


# --- 3. Preparar o dataframe de atualizações (dados raspados com sucesso) ---
# Este passo é o mesmo de antes, garantindo que df_dados_raspados_para_atualizar contém dados limpos.
updates_miad <- df_final_musicas_iguais_artistas_diferentes %>%
  select(ID = Codigo, Nome.y, Artista.y, Letra)

updates_maig <- df_final_musicas_e_artistas_iguais %>%
  select(ID = Codigo, Nome.y, Artista.y, Letra)

df_dados_raspados_para_atualizar <- bind_rows(updates_miad, updates_maig) %>%
  distinct(ID, .keep_all = TRUE)

# Verificar e remover linhas com NA antes da atualização (como solicitado antes)
original_rows_in_update <- nrow(df_dados_raspados_para_atualizar)
df_dados_raspados_para_atualizar <- df_dados_raspados_para_atualizar %>%
  filter(!is.na(Nome.y), !is.na(Artista.y), !is.na(Letra))
rows_removed_from_update <- original_rows_in_update - nrow(df_dados_raspados_para_atualizar)
if (rows_removed_from_update > 0) {
  print(paste0(rows_removed_from_update, " linhas com NA foram removidas de df_dados_raspados_para_atualizar antes da atualização."))
} else {
  print("Nenhuma linha com NA foi encontrada em df_dados_raspados_para_atualizar.")
}

# --- 4. Aplicar as atualizações no dataframe principal (data_final_pt) ---
# Criar uma cópia de data_final_pt para não modificar o original
data_final_com_atualizacoes <- data_final_pt

# Aplicar as atualizações em data_final_com_atualizacoes
# CORREÇÃO AQUI: A variável correta é data_final_com_atualizacoes
data_final_com_atualizacoes <- data_final_com_atualizacoes %>%
  rows_update(df_dados_raspados_para_atualizar, by = "ID", unmatched = "ignore")

# Inspecionar o dataframe após as atualizações (opcional)
print("Dataframe 'data_final_com_atualizacoes' (data_final_pt com as atualizações aplicadas):")
print(head(data_final_com_atualizacoes))
print(dim(data_final_com_atualizacoes))


# --- 5. Filtrar o dataframe principal para ANÁLISE FINAL ---
# Remover do dataframe principal APENAS as músicas que PERMANECERAM em df_musicas_retiradas_detalhado_final
df_final_para_analise_filtrado <- data_final_com_atualizacoes %>%
  filter(!(ID %in% df_musicas_retiradas_detalhado_final$Codigo))

# Inspecionar o dataframe filtrado para análise (opcional)
print("Dataframe 'df_final_para_analise_filtrado' (antes da seleção final de colunas):")
print(head(df_final_para_analise_filtrado))
print(dim(df_final_para_analise_filtrado))


# --- 6. Finalizar o dataframe principal para análise: Selecionar e reorganizar colunas ---
df_final_para_analise <- df_final_para_analise_filtrado %>%
  select(
    Codigo = ID,      # Renomeia ID para Codigo
    Nome_Original = Nome.x,
    Artista_Original = Artista.x,
    Ano,
    Nome_Site = Nome.y,     # Nome da música (atualizado) do site
    Artista_Site = Artista.y, # Nome do artista (atualizado) do site
    Letra_Site = Letra      # Letra da música (atualizada) do site
    # As colunas '...1' e 'Idioma' serão automaticamente excluídas por não estarem listadas
  )

# Inspecionar o dataframe final para análise
print("Dataframe Final Pronto para Análise (com a nova ordem, atualizações e filtragem final):")
View(df_final_para_analise)
print(dim(df_final_para_analise))

# Opcional: Salvar os novos dataframes
# write.csv(df_musicas_retiradas_detalhado_final, "df_musicas_retiradas_final.csv", row.names = FALSE, fileEncoding = "UTF-8")
# write.csv(df_final_para_analise, "data_final_pronto_para_analise_revisado.csv", row.names = FALSE, fileEncoding = "UTF-8")