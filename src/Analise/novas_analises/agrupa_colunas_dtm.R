# Certifique-se de que dtm_letras_tf já foi criada e está na memória.

library(dplyr)
library(lsa)
library(tidyr)
library(ggplot2)
library(purrr)
library(scales) # Para scales::hue_pal()
library(tidytext) # Para unnest_tokens
library(Matrix) # Para criar a matriz esparsa
library(tm) # Necessário para a classe DocumentTermMatrix


# --- 1. Análise de Frequência Total dos Termos ---
matrix_dtm_original <- as.matrix(dtm_letras_tf)
frequencia_total_palavras <- colSums(matrix_dtm_original)


# --- 2. Definir as Faixas de Frequência e Classificar os Termos ---
min_frequencia_keep <- 31
max_frequencia_keep <- 1000

df_termos_frequencia <- data.frame(
  termo = names(frequencia_total_palavras),
  frequencia = as.numeric(frequencia_total_palavras),
  stringsAsFactors = FALSE
)

df_termos_classificados <- df_termos_frequencia %>%
  mutate(
    grupo_filtragem = case_when(
      frequencia == 1 ~ "bin_1_vez",
      frequencia >= 2 & frequencia <= 5 ~ "bin_2_a_5",
      frequencia >= 6 & frequencia <= 15 ~ "bin_6_a_15",
      frequencia >= 16 & frequencia <= 30 ~ "bin_16_a_30",
      TRUE ~ "manter_individual"
    )
  )


# --- 3. CRIAR A DTM FILTRADA CORRETAMENTE (APLICANDO OS MESMOS FILTROS) ---
# Agora, a lógica de filtragem é a mesma em todos os lugares.
termos_a_manter <- df_termos_classificados %>%
  filter(grupo_filtragem == "manter_individual" & frequencia <= max_frequencia_keep) %>%
  pull(termo)

dtm_filtrada <- dtm_letras_tf[, colnames(dtm_letras_tf) %in% termos_a_manter, drop = FALSE]

print("Etapa 3: DTM filtrada por frequência (dtm_filtrada) criada.")
print(paste0("Dimensões: ", paste(dim(dtm_filtrada), collapse = "x")))


# --- 4. Criar as Colunas Agregadas dos Termos a serem Filtrados ---
colunas_agregadas <- data.frame(row.names = row.names(dtm_letras_tf))
bins <- unique(df_termos_classificados$grupo_filtragem[df_termos_classificados$grupo_filtragem != "manter_individual"])

for (bin_name in bins) {
  termos_no_bin <- df_termos_classificados %>% filter(grupo_filtragem == bin_name) %>% pull(termo)
  if (length(termos_no_bin) > 0) {
    dtm_sub <- dtm_letras_tf[, termos_no_bin, drop = FALSE]
    colunas_agregadas[, bin_name] <- rowSums(as.matrix(dtm_sub))
  } else {
    colunas_agregadas[, bin_name] <- 0
  }
}

# --- 5. Combinar as DTMs ---
dtm_matriz_combinada <- cbind(as.matrix(dtm_filtrada), as.matrix(colunas_agregadas))

row.names(dtm_matriz_combinada) <- row.names(dtm_letras_tf)


# --- 6. CONVERTER PARA O FORMATO DTM ---
dtm_hibrida <- as.DocumentTermMatrix(dtm_matriz_combinada, weighting = weightTf)

dimnames(dtm_hibrida) <- list(Docs = row.names(dtm_matriz_combinada), Terms = colnames(dtm_matriz_combinada))


# --- 7. Inspecionar o resultado final ---
print("\n--- DTM Híbrida Criada com Sucesso (Termos Individuais + Bins Agregados) ---")
print("Dimensões da DTM Híbrida:")
print(dim(dtm_hibrida))
print(paste("Número de colunas individuais mantidas (> 30 e <= 1000):", ncol(dtm_filtrada)))
print(paste("Número de colunas de bins agregadas:", ncol(colunas_agregadas)))
print("Nomes das colunas (uma mistura de termos e bins):")
print(head(colnames(dtm_hibrida), 20))
View(as.matrix(dtm_hibrida)[1:10, 1:20])

# --- VERIFICAÇÃO FINAL ---
if (ncol(dtm_hibrida) == ncol(dtm_filtrada) + ncol(colunas_agregadas)) {
  cat("\nVERIFICAÇÃO FINAL: OK. A DTM híbrida tem o número de colunas esperado (", ncol(dtm_filtrada) + ncol(colunas_agregadas), ").\n")
} else {
  cat("\nERRO GRAVE NA CONSTRUÇÃO: O número de colunas da DTM híbrida está incorreto.\n")
}