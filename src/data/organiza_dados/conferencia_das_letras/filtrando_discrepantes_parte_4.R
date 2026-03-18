library(dplyr)
library(purrr) # Para bind_rows

# Certifique-se de que os seguintes dataframes (e funções) estão carregados em seu ambiente R:
# - data_final_pt (seu dataframe ORIGINAL - usado para detalhes das músicas retiradas)
# - df_final_para_analise_round2 (o dataframe a ser filtrado e atualizado - resultado da rodada 1)
#
# - df_consolidado_limpo_from_ma_diff (do processamento do CASE 2)
# - df_final_musicas_iguais_artistas_diferentes_from_ma_diff (do processamento do CASE 2)
# - df_final_musicas_e_artistas_diferentes_from_ma_diff (do processamento do CASE 2)
# - df_final_musicas_e_artistas_iguais_from_ma_diff (do processamento do CASE 2)
# - df_final_dados_web_nao_encontrados_from_ma_diff (do processamento do CASE 2)
#
# - As funções: remove_acentos e clean_string_for_comparison (já devem estar definidas)


# --- FUNÇÕES DE AJUDA (repetidas para garantir disponibilidade, comente se já definidas) ---
# Função auxiliar para remover acentos
remove_acentos <- function(texto) {
  if (is.character(texto)) {
    chartr(
      "áéíóúÁÉÍÓÚàèìòùÀÈÌÒÙãõñÃÕÑâêîôûÂÊÎÔÛçÇ",
      "aeiouAEIOUaeiouAEIOUaonAONaeiouAEIOUcC",
      texto
    )
  } else {
    texto
  }
}

# Função de limpeza abrangente para comparação
clean_string_for_comparison <- function(text) {
  if (is.character(text)) {
    text <- tolower(trimws(text))
    text <- gsub("\\s*\\([^\\)]*\\)", "", text)
    text <- gsub("[^[:alnum:]\\s]", "", text)
    text <- gsub("[[:digit:]]", "", text)
    text <- chartr(
      "áéíóúÁÉÍÓÚàèìòùÀÈÌÒÙãõñÃÕÑâêîôûÂÊÎÔÛçÇ",
      "aeiouAEIOUaeiouAEIOUaonAONaeiouAEIOUcC",
      text
    )
    text <- gsub("\\s+", " ", text)
    text <- trimws(text)
  }
  return(text)
}

#=====================================================================================================================

# --- 1. IDENTIFICAR MÚSICAS PARA REMOÇÃO NESTA RODADA (CASE 2) ---

# 1.1. Coletar IDs inicialmente marcados para remoção do CASE 2
ids_para_remocao_inicial_this_round <- unique(c(
  df_final_dados_web_nao_encontrados_from_ma_diff$Codigo,
  df_final_musicas_e_artistas_diferentes_from_ma_diff$Codigo
))

print(paste0("Número de IDs inicialmente marcados para remoção (SOMENTE ESTA RODADA): ", length(ids_para_remocao_inicial_this_round)))


# 1.2. Criar o dataframe df_musicas_retiradas_detalhado_round2 (com originais e raspados) ---
# Filtrar o dataframe ORIGINAL (data_final_pt) pelas músicas a serem retiradas nesta rodada
df_base_para_retiradas_round2 <- data_final_pt %>%
  filter(ID %in% ids_para_remocao_inicial_this_round)

# Juntar este subconjunto com o df_consolidado_limpo_from_ma_diff para adicionar os valores raspados do CASE 2
df_musicas_retiradas_detalhado_round2 <- df_base_para_retiradas_round2 %>%
  left_join(
    df_consolidado_limpo_from_ma_diff %>%
      select(Codigo, Nome.y_raspado = Nome.y, Artista.y_raspado = Artista.y, Letra_raspada = Letra),
    by = c("ID" = "Codigo") # Junção pelo ID
  ) %>%
  select(
    `...1`,
    Codigo = ID,
    Nome_Original = Nome.x,
    Artista_Original = Artista.x,
    Ano,
    # --- CORREÇÃO AQUI: REMOVIDO O '.x' ---
    Nome_Original_Y = Nome.y,       # Agora é Nome.y (do df_base_para_retiradas_round2)
    Artista_Original_Y = Artista.y, # Agora é Artista.y (do df_base_para_retiradas_round2)
    Letra_Original = Letra,         # Agora é Letra (do df_base_para_retiradas_round2)
    # --- FIM DA CORREÇÃO ---
    Nome_Raspado = Nome.y_raspado,    # Nome da música raspado do site
    Artista_Raspado = Artista.y_raspado, # Artista raspado do site
    Letra_Raspada = Letra_raspada,    # Letra raspada do site
    Idioma
  )

print(paste0("Dimensão de df_musicas_retiradas_detalhado_round2 (antes da verificação final): ", 
             paste(dim(df_musicas_retiradas_detalhado_round2), collapse = "x")))


# --- Verificação e Ajuste Final do df_musicas_retiradas_detalhado_round2 ---

# 2.1. Identificar IDs das músicas que devem "voltar" para a base principal (desta rodada)
# Aplicar a limpeza abrangente antes da comparação
ids_para_retornar_a_base_round2 <- df_musicas_retiradas_detalhado_round2 %>%
  mutate(
    Nome_Original_Comp = sapply(Nome_Original, clean_string_for_comparison),
    Nome_Original_Y_Comp = sapply(Nome_Original_Y, clean_string_for_comparison)
  ) %>%
  filter(
    !is.na(Nome_Original_Comp),
    !is.na(Nome_Original_Y_Comp),
    Nome_Original_Comp == Nome_Original_Y_Comp
  ) %>%
  pull(Codigo)

print(paste0("Número de IDs que retornarão para a base principal (SOMENTE ESTA RODADA): ", length(ids_para_retornar_a_base_round2)))

# 2.2. Ajustar df_musicas_retiradas_detalhado_round2: remover as músicas que estão "voltando"
df_musicas_retiradas_detalhado_round2_final <- df_musicas_retiradas_detalhado_round2 %>%
  filter(!(Codigo %in% ids_para_retornar_a_base_round2))

print(paste0("Dimensão FINAL de df_musicas_retiradas_detalhado_round2_final: ", 
             paste(dim(df_musicas_retiradas_detalhado_round2_final), collapse = "x")))


#=====================================================================================================================
# --- 3. FILTRAR O DATAFRAME PRINCIPAL (df_final_para_analise) ---

# Remover do df_final_para_analise APENAS as músicas que PERMANECERAM
# em df_musicas_retiradas_detalhado_this_round
df_para_analise_pos_remocao_round2 <- df_final_para_analise %>%
  filter(!(Codigo %in% ids_finalmente_removidos_this_round))

print("Dimensão do dataframe principal após remoção (df_para_analise_pos_remocao_round2):")
print(dim(df_para_analise_pos_remocao_round2))


#=====================================================================================================================
# --- 4. PREPARAR DADOS PARA ATUALIZAÇÃO NESTA RODADA ---

# Coletar dados de atualização de sucesso do CASE 2
updates_case2_miad <- df_final_musicas_iguais_artistas_diferentes_from_ma_diff %>%
  select(Codigo, Nome.y, Artista.y, Letra)
updates_case2_maig <- df_final_musicas_e_artistas_iguais_from_ma_diff %>%
  select(Codigo, Nome.y, Artista.y, Letra)

# Combinar dados de atualização desta rodada (CASE 2)
df_dados_raspados_para_atualizar_this_round <- bind_rows(updates_case2_miad, updates_case2_maig) %>%
  distinct(Codigo, .keep_all = TRUE)

# Verificar e remover linhas com NA
original_rows_in_this_update <- nrow(df_dados_raspados_para_atualizar_this_round)
df_dados_raspados_para_atualizar_this_round_limpo <- df_dados_raspados_para_atualizar_this_round %>%
  filter(!is.na(Nome.y), !is.na(Artista.y), !is.na(Letra))
rows_removed_from_this_update <- original_rows_in_this_update - nrow(df_dados_raspados_para_atualizar_this_round_limpo)
if (rows_removed_from_this_update > 0) {
  print(paste0(rows_removed_from_this_update, " linhas com NA foram removidas dos dados de atualização desta rodada."))
} else {
  print("Nenhuma linha com NA foi encontrada nos dados de atualização desta rodada.")
}


#=====================================================================================================================
# --- 5. REALIZAR ATUALIZAÇÃO NESTA RODADA NO DATAFRAME PRINCIPAL ---

# Renomear colunas para corresponderem às do df_para_analise_pos_remocao_round2
df_updates_formatted_this_round <- df_dados_raspados_para_atualizar_this_round_limpo %>%
  rename(
    Nome_Site = Nome.y,
    Artista_Site = Artista.y,
    Letra_Site = Letra
  )

# Aplicar as atualizações no dataframe principal (já filtrado)
# O df_final_para_analise_total é o resultado final desta rodada
df_final_para_analise_total <- df_para_analise_pos_remocao_round2 %>%
  rows_update(df_updates_formatted_this_round, by = "Codigo", unmatched = "ignore")


# Inspecionar o dataframe final após remoção e atualização desta rodada
print("Dataframe Final PRONTO PARA ANÁLISE (df_final_para_analise_total):")
View(df_final_para_analise_total)
print(dim(df_final_para_analise_total))


#=====================================================================================================================
# --- 6. FINALIZAR O DATAFRAME PRINCIPAL PARA ANÁLISE ---

# Selecionar e reorganizar as colunas finais
df_final_para_analise_total <- df_final_para_analise_total %>%
  select(
    Codigo = Codigo, # Codigo já está com o nome correto
    Nome_Original = Nome_Original, # Nome_Original já está com o nome correto
    Artista_Original = Artista_Original, # Artista_Original já está com o nome correto
    Ano, # Ano já está com o nome correto
    Nome_Site = Nome_Site,     # Nome da música (atualizado) do site
    Artista_Site = Artista_Site, # Nome do artista (atualizado) do site
    Letra_Site = Letra_Site      # Letra da música (atualizada) do site
  )
# As colunas não listadas (como Nome.x, Artista.x, Nome.y, Artista.y, etc., e as _limpo_final)
# serão automaticamente excluídas se não forem listadas aqui.

print("Dataframe Final PRONTO PARA ANÁLISE (df_final_para_analise_total):")
View(df_final_para_analise_total)
print(dim(df_final_para_analise_total))

View(df_musicas_retiradas_detalhado_this_round)
dim(df_musicas_retiradas_detalhado_this_round)

# Opcional: Salvar os dataframes finais desta rodada
# write.csv(df_musicas_retiradas_detalhado_this_round, "df_musicas_retiradas_final_round2.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_final_para_analise_total, "df_final_analise_completa_round2.csv", row.names = FALSE, fileEncoding = "UTF-8")

