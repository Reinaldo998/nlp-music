library(dplyr)

# Certifique-se de que a matriz 'matriz_sim_cosseno_total' está na sua sessão.

# --- 1. Encontre os pares com similaridade 1 na matriz completa ---
tolerancia_erro <- 1e-9
pares_sim1_indices <- which(abs(matriz_sim_cosseno_total - 1) < tolerancia_erro, arr.ind = TRUE)

# --- 2. Remover a diagonal principal e os pares duplicados (ex: [1,2] e [2,1]) ---
df_pares_sim1_indices <- as.data.frame(pares_sim1_indices) %>%
  filter(row != col) %>%
  filter(row < col)

# --- 3. Mapear os índices de volta para os Códigos ---
if (nrow(df_pares_sim1_indices) > 0) {
  if (!exists("data_com_topicos_atual")) {
    stop("O dataframe 'data_com_topicos_atual' não está disponível para mapeamento dos códigos.")
  }
  
  codigos_da_matriz <- row.names(matriz_sim_cosseno_total)
  
  df_pares_sim1_total <- df_pares_sim1_indices %>%
    mutate(
      Codigo_musica1 = as.numeric(codigos_da_matriz[row]),
      Codigo_musica2 = as.numeric(codigos_da_matriz[col])
    ) %>%
    # --- CORREÇÃO AQUI: USAR OS NOMES DE COLUNA CORRETOS ---
    left_join(
      data_com_topicos_atual %>% select(ID, Nome.x, Artista.x),
      by = c("Codigo_musica1" = "ID")
    ) %>%
    rename(
      Nome_Original_1 = Nome.x,
      Artista_Original_1 = Artista.x
    ) %>%
    left_join(
      data_com_topicos_atual %>% select(ID, Nome.x, Artista.x),
      by = c("Codigo_musica2" = "ID")
    ) %>%
    rename(
      Nome_Original_2 = Nome.x,
      Artista_Original_2 = Artista.x
    ) %>%
    select(
      Nome_Original_1, Artista_Original_1, Codigo_musica1,
      Nome_Original_2, Artista_Original_2, Codigo_musica2
    )
  
  print("Pares de músicas com similaridade 1 na base completa:")
  View(df_pares_sim1_total)
  print(df_pares_sim1_total)
  
} else {
  print("Nenhum par de músicas com similaridade 1 foi encontrado na matriz completa.")
}

View(data_com_topicos_atual %>%
       filter(ID %in% c(6015,6071,6259,6478,7575,7158,7206,9768,7263,7221,7211,7166,1776,9848,439,529,9768,7158,55)))
