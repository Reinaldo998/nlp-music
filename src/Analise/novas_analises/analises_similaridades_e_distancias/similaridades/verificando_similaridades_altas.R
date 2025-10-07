#=====================================================================================================
library(dplyr)
library(lsa)

# --- 1. Defina o tópico que você quer investigar ---
# ATENÇÃO: Substitua 'SEU_TOPICO_AQUI' pelo número do tópico que você encontrou em 'analise_similaridade_resumo'
topico_alvo <- 10 # Exemplo: 15

# --- 2. Obtenha os Códigos das músicas que pertencem ao tópico alvo ---
# O dataframe correto para este pipeline é 'data_com_topicos_atual' e a coluna é 'topico_lda'
musicas_do_topico <- data_com_topicos_atual %>%
  filter(topico_lda == topico_alvo)

# Verificação: se não houver músicas, pare a execução
if (nrow(musicas_do_topico) < 2) {
  stop(paste0("O Tópico ", topico_alvo, " tem menos de 2 músicas. Similaridade não calculável."))
}

# --- 3. Subconjunto da Matriz de Similaridade Total ---
# A matriz de similaridade já está calculada para todas as músicas.
# Vamos filtrar a matriz para pegar apenas as linhas e colunas
# que correspondem às músicas do tópico alvo.
# O dataframe 'musicas_do_topico' agora tem a coluna 'Codigo'
codigos_do_topico <- as.character(musicas_do_topico$ID)
matriz_sim_cosseno_topico_alvo <- matriz_sim_cosseno_total[codigos_do_topico, codigos_do_topico]

# --- 4. Encontre os Pares com Similaridade Exatamente 1 (ou muito próxima) ---
# Usamos a mesma lógica de tolerância para evitar erros de ponto flutuante.
tolerancia_erro <- 1e-19
pares_sim1 <- which(abs(matriz_sim_cosseno_topico_alvo - 1) < tolerancia_erro, arr.ind = TRUE)

# A diagonal da matriz é sempre 1. Removemos a diagonal para encontrar pares
# de músicas diferentes que têm similaridade 1.
pares_sim1 <- pares_sim1[pares_sim1[, "row"] != pares_sim1[, "col"], ]

# --- 5. Exibir os resultados ---
cat(paste0("\n--- PARES DE MÚSICAS NO TÓPICO ", topico_alvo, " COM SIMILARIDADE DE 1 ---\n"))

if (nrow(pares_sim1) > 0) {
  for (i in 1:nrow(pares_sim1)) {
    # Obtemos os códigos das músicas diretamente dos nomes das linhas da matriz
    codigo1 <- as.numeric(row.names(matriz_sim_cosseno_topico_alvo)[pares_sim1[i, "row"]])
    codigo2 <- as.numeric(row.names(matriz_sim_cosseno_topico_alvo)[pares_sim1[i, "col"]])
    
    # Fazemos uma consulta rápida ao dataframe original para pegar o Nome e o Artista
    musica1 <- musicas_do_topico %>% filter(Codigo == codigo1)
    musica2 <- musicas_do_topico %>% filter(Codigo == codigo2)
    
    cat(paste0(
      "Par ", i, ":\n",
      " - Música 1: '", musica1$Nome_Original, "' de ", musica1$Artista_Original, " (Código: ", musica1$Codigo, ")\n",
      " - Música 2: '", musica2$Nome_Original, "' de ", musica2$Artista_Original, " (Código: ", musica2$Codigo, ")\n",
      " - Similaridade Calculada: 1.0\n\n"
    ))
  }
} else {
  cat("Nenhum par de músicas com similaridade 1 foi encontrado no Tópico ", topico_alvo, ".\n")
}

#=====================================================================================================================

# Certifique-se de que o dataframe 'df_similaridades_longo' está na sua sessão R.
# Ele é o dataframe que contém todos os scores de similaridade calculados.

# Salve a opção de dígitos original para restaurar depois
original_digits <- getOption("digits")

# Defina a precisão para um valor alto, por exemplo 16 dígitos
options(digits = 16)

# --- Verificando o valor máximo de similaridade real para o Tópico 15 ---

# Filtra o dataframe de similaridades para o Tópico 15
sims_topico_15 <- df_similaridades_longo %>%
  filter(topico == 15)

# Pega o valor máximo real, sem arredondamento
max_value <- max(sims_topico_15$similaridade)

print(paste0("O valor máximo real de similaridade para o Tópico 15 é: ", max_value))
print(paste0("O valor máximo é exatamente 1? ", isTRUE(all.equal(max_value, 1))))


# --- Verificando os pares de músicas que têm essa similaridade máxima ---

# Encontre os pares de músicas que têm similaridade igual a 'max_value'
# A 'matriz_sim_cosseno_total' deve estar na sessão para este passo.
if (exists("matriz_sim_cosseno_total")) {
  # Filtra a matriz para pegar só as linhas e colunas do tópico 15
  codigos_do_topico <- as.character(musicas_do_topico$Codigo)
  matriz_do_topico <- matriz_sim_cosseno_total[codigos_do_topico, codigos_do_topico]
  
  # Encontrar os índices na matriz que têm o valor máximo
  pares_com_valor_max <- which(abs(matriz_do_topico - max_value) < 1e-15, arr.ind = TRUE)
  
  # Remove a diagonal
  pares_com_valor_max <- pares_com_valor_max[pares_com_valor_max[, "row"] != pares_com_valor_max[, "col"], ]
  
  if (nrow(pares_com_valor_max) > 0) {
    print("\nCódigos dos pares com a similaridade máxima:")
    for(i in 1:nrow(pares_com_valor_max)) {
      musica1 <- musicas_do_topico[pares_com_valor_max[i, "row"], ]
      musica2 <- musicas_do_topico[pares_com_valor_max[i, "col"], ]
      cat(paste0(" - Par: (", musica1$Codigo, ", ", musica2$Codigo, ")\n"))
    }
  } else {
    print("\nNenhum par com a similaridade máxima foi encontrado (além da diagonal).")
  }
} else {
  print("\nAVISO: 'matriz_sim_cosseno_total' não está disponível para verificar os pares.")
}


# Restaure as opções de precisão originais
options(digits = original_digits)
#==============================================================================================================
library(dplyr)
library(lsa)

# --- 1. Defina o tópico que você quer investigar ---
topico_alvo <- 16 # Exemplo: Tópico 15

# --- 2. Obtenha as músicas do tópico e garanta que a coluna ID seja Codigo ---
musicas_do_topico <- data_com_topicos_atual %>%
  filter(topico_lda == topico_alvo)

# Verificação: se não houver músicas suficientes, pare a execução
if (nrow(musicas_do_topico) < 2) {
  stop(paste0("O Tópico ", topico_alvo, " tem menos de 2 músicas. A similaridade não pode ser calculada."))
}

# --- 3. Subconjunto da Matriz de Similaridade Total ---
# Usamos a coluna ID para indexar a matriz
codigos_do_topico <- as.character(musicas_do_topico$ID)
matriz_sim_cosseno_topico_alvo <- matriz_sim_cosseno_total[codigos_do_topico, codigos_do_topico]

# --- 4. Encontrar o par de similaridade MÁXIMA ---
sim_pares <- matriz_sim_cosseno_topico_alvo[upper.tri(matriz_sim_cosseno_topico_alvo)]
max_similaridade <- max(sim_pares)
tolerancia_erro <- 1e-9
indices_max_sim <- which(abs(matriz_sim_cosseno_topico_alvo - max_similaridade) < tolerancia_erro, arr.ind = TRUE)
indices_max_sim <- indices_max_sim[indices_max_sim[, "row"] != indices_max_sim[, "col"], ]

# --- 5. Exibir o resultado ---
cat(paste0("\n--- PAR DE MÚSICAS COM A MÁXIMA SIMILARIDADE (Tópico ", topico_alvo, ") ---\n"))
cat(paste0("Valor máximo de similaridade encontrado: ", max_similaridade, "\n\n"))

if (nrow(indices_max_sim) > 0) {
  row_idx1 <- indices_max_sim[1, "row"]
  row_idx2 <- indices_max_sim[1, "col"]
  
  # Mapeia os índices de volta para os IDs
  id1 <- as.numeric(row.names(matriz_sim_cosseno_topico_alvo)[row_idx1])
  id2 <- as.numeric(row.names(matriz_sim_cosseno_topico_alvo)[row_idx2])
  
  musica1 <- musicas_do_topico %>% filter(ID == id1)
  musica2 <- musicas_do_topico %>% filter(ID == id2)
  
  cat(paste0(
    " - Música 1: '", musica1$Nome_Original, "' de ", musica1$Artista_Original, " (ID: ", musica1$ID, ")\n",
    " - Música 2: '", musica2$Nome_Original, "' de ", musica2$Artista_Original, " (ID: ", musica2$ID, ")\n"
  ))
} else {
  cat("Nenhum par de músicas com similaridade máxima foi encontrado neste tópico (além da diagonal).\n")
}

View(data_com_topicos_atual %>%
       filter(ID %in% c(6015,6071,7263,7221,7211,7166,1776,9848,439,529,9768,7158)))
