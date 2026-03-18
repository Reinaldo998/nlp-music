library(dplyr) # Certifique-se que dplyr está carregado

atualizado_na_mao<-c(5814,5836,5888,6028,6095,6397,6564,6678,7166,7644,192,5962,5876,5930,5872,5976,6048,6148,6259)
# --- 0. Defina os detalhes da MÚSICA ESPECÍFICA que você quer atualizar ---
# ATENÇÃO: Substitua esses valores pelos CÓDIGOS E NOVA LETRA REAIS
CODIGO_DA_MUSICA_A_ATUALIZAR <- 7071# O Código da música que você quer corrigir
NOVA_LETRA_PARA_ESSA_MUSICA <-''
# Lembre-se de usar aspas simples externas (') se a sua letra tiver aspas duplas internas (")

NOVA_LETRA_PARA_ESSA_MUSICA 

# --- 1. Opcional: Inspecionar a música ANTES da atualização ---
print("Música ANTES da atualização:")
#View(df_final_para_analise_total %>% filter(Codigo == CODIGO_DA_MUSICA_A_ATUALIZAR))
print(df_final_total_combinado_simples %>% filter(Codigo == CODIGO_DA_MUSICA_A_ATUALIZAR))


# --- 2. Realizar a Atualização da MÚSICA ESPECÍFICA ---
# O dataframe df_final_para_analise_total será diretamente modificado.
df_final_total_combinado_simples <- df_final_total_combinado_simples %>%
  mutate(
    # Atualiza Nome_Site: se o Codigo corresponder, usa Nome_Original, senão mantém Nome_Site existente
    Nome_Site = case_when(
      Codigo == CODIGO_DA_MUSICA_A_ATUALIZAR ~ Nome_Original,
      TRUE ~ Nome_Site # Mantém o Nome_Site original para outras músicas
    ),
    # Atualiza Artista_Site: se o Codigo corresponder, usa Artista_Original, senão mantém Artista_Site existente
    Artista_Site = case_when(
      Codigo == CODIGO_DA_MUSICA_A_ATUALIZAR ~ Artista_Original,
      TRUE ~ Artista_Site # Mantém o Artista_Site original para outras músicas
    ),
    # Atualiza Letra_Site: se o Codigo corresponder, usa a NOVA_LETRA_PARA_ESSA_MUSICA, senão mantém Letra_Site existente
    Letra_Site = case_when(
      Codigo == CODIGO_DA_MUSICA_A_ATUALIZAR ~ NOVA_LETRA_PARA_ESSA_MUSICA,
      TRUE ~ Letra_Site # Mantém a Letra_Site original para outras músicas
    )
  )

# --- 3. Opcional: Inspecionar a música APÓS a atualização ---
print("Música APÓS a atualização:")
#View(df_final_para_analise_total %>% filter(Codigo == CODIGO_DA_MUSICA_A_ATUALIZAR))
print(df_final_total_combinado_simples %>% filter(Codigo == CODIGO_DA_MUSICA_A_ATUALIZAR))

