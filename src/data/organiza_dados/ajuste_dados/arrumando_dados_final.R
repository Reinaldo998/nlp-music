library(dplyr)
library(purrr) # Para bind_rows

# Certifique-se de que os seguintes dataframes estão carregados em seu ambiente R:
# - df_final_para_analise_total (seu dataframe principal "bom" e atualizado)
# - df_musicas_retiradas_detalhado_this_round (o dataframe de músicas retiradas desta rodada)


# --- 1. Preparar df_musicas_retiradas_detalhado_this_round para a junção ---
# Renomear colunas e selecionar apenas as necessárias para combinar com df_final_para_analise_total.
df_retiradas_para_combinar <- df_musicas_retiradas_detalhado_this_round %>%
  select(
    Codigo = ID,          # Renomeia ID para Codigo
    Nome_Original = Nome.x,     # Renomeia Nome.x para Nome_Original
    Artista_Original = Artista.x, # Renomeia Artista.x para Artista_Original
    Ano = Ano,            # Mantém Ano
    Nome_Site = Nome.y,         # Renomeia Nome.y para Nome_Site
    Artista_Site = Artista.y,   # Renomeia Artista.y para Artista_Site
    Letra_Site = Letra          # Renomeia Letra para Letra_Site
    # Colunas ...1 e Idioma são excluídas automaticamente por não serem selecionadas
    # Colunas Nome_Original_Y, Artista_Original_Y, Letra_Original, Nome_Raspado, Artista_Raspado, Letra_Raspada
    # também não são incluídas aqui para simplificar a junção.
  )

print("df_retiradas_para_combinar (preparado para junção):")
View(df_retiradas_para_combinar)
print(dim(df_retiradas_para_combinar))


# --- 2. Juntar os dois dataframes e remover duplicatas de Codigo ---
# bind_rows empilha os dataframes. Se houver Codigos em comum, eles serão duplicados.
# distinct(Codigo, .keep_all = TRUE) remove essas duplicatas.
# Como df_final_para_analise_total vem primeiro, suas linhas serão preferidas em caso de Codigo duplicado.
df_final_total_combinado_simples <- bind_rows(
  df_final_para_analise_total,
  df_retiradas_para_combinar
) %>%
  distinct(Codigo, .keep_all = TRUE)


print("df_final_total_combinado_simples (Dataframe Final Combinado e Desduplicado):")
View(df_final_total_combinado_simples)
print(dim(df_final_total_combinado_simples))

# Opcional: Salvar o dataframe final
# write.csv(df_final_total_combinado_simples, "df_analise_total_combinado_simples.csv", row.names = FALSE, fileEncoding = "UTF-8")

#=====================================================================================================================
# --- NOVO PASSO: ANULAÇÃO MANUAL DE DADOS DO SITE (Nome_Site, Artista_Site, Letra_Site) ---

# 1. Defina os Códigos das músicas que você quer ANULAR manualmente
# ATENÇÃO: Substitua os exemplos pelos CÓDIGOS REAIS das suas músicas
#codigos_para_anular_manualmente <- c(5831, 5894, 5959, 6038,6046, 6057,6076,6168,6171,6266,6267,6280,6348,6351,6357,6367,6392,6396,6497,6499,6561,6570,6674,6799,7087,7182,7298,
#                                     7594,7672,7998) # Exemplo: Códigos para anular

#codigos_para_anulados <- c(5831, 5894, 5959, 6038,6046, 6057,6076,6168,6171,6266,6267,6280,6348,6351,6357,6367,6392,6396,6497,6499,6561,6570,6674,6799,7087,7182,7298,
#                                     7594,7672,7998,6252,6797,6193,5985,5921,5932,6211,6255,6389,6685,66100,6760,6771)

length(codigos_para_anulados)
codigos_para_anular_manualmente <-ids_a_remover #c(6071,7263,9768,55,529,7211,6478,7575)#c(6252,6797,6193)# c(5985,5921,5932,6211,6255,6389,6685,66100,6760,6771) # Exemplo: Códigos para anular

# 2. Realizar a anulação das colunas para os Códigos especificados
df_final_total_combinado_simples <- df_final_total_combinado_simples %>%
  mutate(
    # Se o Codigo da linha estiver no vetor de codigos_para_anular_manualmente,
    # defina Nome_Site como NA, senão mantenha o valor atual.
    Nome_Site = case_when(
      Codigo %in% codigos_para_anular_manualmente ~ NA_character_, # Use NA_character_ para colunas de texto
      TRUE ~ Nome_Site
    ),
    # Faça o mesmo para Artista_Site
    Artista_Site = case_when(
      Codigo %in% codigos_para_anular_manualmente ~ NA_character_,
      TRUE ~ Artista_Site
    ),
    # Faça o mesmo para Letra_Site
    Letra_Site = case_when(
      Codigo %in% codigos_para_anular_manualmente ~ NA_character_,
      TRUE ~ Letra_Site
    )
  )

# Inspecionar o dataframe após a anulação manual (opcional)
print("df_final_total_combinado_simples após anulação manual:")
View(df_final_total_combinado_simples %>% filter(Codigo %in% codigos_para_anular_manualmente))
print(df_final_total_combinado_simples %>% filter(Codigo %in% codigos_para_anular_manualmente))


# Opcional: Salvar o dataframe final
#write.csv(df_final_total_combinado_simples, "df_analise_total_combinado_simples.csv", row.names = FALSE, fileEncoding = "UTF-8")

dim(df_retiradas_para_combinar %>% filter(Codigo %in% codigos_para_anular_manualmente))
dim(df_retiradas_para_combinar)

View(df_retiradas_para_combinar %>%
       filter(!(Codigo %in% codigos_para_anular_manualmente)))




