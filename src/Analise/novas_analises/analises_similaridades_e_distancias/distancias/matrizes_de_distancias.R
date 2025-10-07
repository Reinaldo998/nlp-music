# --- Carregar os pacotes necessários ---
library(philentropy) # Para calcular diversas distâncias
library(lsa)          # Mantido para a similaridade de cosseno
library(Matrix)       # Para manipulação de matrizes esparsas, se necessário

# Assumimos que a matriz 'dtm_filtrada' foi criada.
# Se 'dtm_tf_normalizada' já existe e não foi a causa do problema,
# você pode usá-la. Mas a recomendação é re-normalizar.

# --- Etapa de Pré-processamento e Normalização Robusta ---
# A DTM para Hellinger e JSD DEVE consistir em distribuições de probabilidade.
# Ou seja, cada linha (música) deve somar 1 e não conter valores negativos/NaN/Inf.

# 1. Converter para matriz densa (se ainda não for)
matrix_dtm <- as.matrix(dtm_filtrada)
#matrix_dtm <- as.matrix(dtm_hibrida)
# 2. Calcular a soma total de palavras por música (soma de cada linha)
total_palavras_por_musica <- rowSums(matrix_dtm)
min(total_palavras_por_musica)
dim(matrix_dtm)
# 3. Identificar e remover músicas que não têm palavras (total_palavras_por_musica == 0)
# Essas músicas causariam divisão por zero e produziriam NaN/Inf.
musicas_com_palavras <- which(total_palavras_por_musica > 0)
matrix_dtm_validas <- matrix_dtm[musicas_com_palavras, ]
total_palavras_validas <- total_palavras_por_musica[musicas_com_palavras]

# Se após a filtragem não houver músicas, parar.
if (nrow(matrix_dtm_validas) == 0) {
  stop("Nenhuma música tem palavras suficientes após a filtragem para calcular as distâncias.")
}

# 4. Normalizar explicitamente cada linha para que a soma seja 1.
# Isso garante que as linhas são distribuições de probabilidade.
dtm_tf_normalizada_pronta <- matrix_dtm_validas / total_palavras_validas
dtm_tf_normalizada<-dtm_tf_normalizada_pronta
# Opcional: Verificar se as somas das linhas agora são 1 (com uma pequena tolerância)
# print(rowSums(dtm_tf_normalizada_pronta[1:min(5, nrow(dtm_tf_normalizada_pronta)),]))

# --- Contexto Importante ---
# Tanto Hellinger quanto Jensen-Shannon (JSD) são distâncias (dissimilaridades)
# e são ideais para comparar distribuições de probabilidade.
# A 'dtm_tf_normalizada_pronta' é agora uma matriz validada para essas métricas.


# --- 1. Calcular a Matriz de Distância de Hellinger ---
# A função 'distance' do pacote 'philentropy' espera que cada linha seja um "ponto".

print("Calculando a Matriz de Distância de Hellinger (0 a 1)...")
# Usaremos 'hellinger_scaled' para obter o resultado diretamente no intervalo [0, 1]
# O pacote philentropy tem uma forma que já escala para [0,1].
# Se você usar 'hellinger' normal, o resultado pode ir até sqrt(2),
# mas 'hellinger_scaled' já faz a divisão por sqrt(2).
matriz_dist_hellinger <- distance(x = dtm_tf_normalizada_pronta, method = "hellinger")

matriz_dist_hellinger <- matriz_dist_hellinger / 2
# Atribuir os nomes das músicas para facilitar a interpretação.
row.names(matriz_dist_hellinger) <- row.names(dtm_tf_normalizada_pronta)
colnames(matriz_dist_hellinger) <- row.names(dtm_tf_normalizada_pronta)


print("Matriz de Distância de Hellinger para todas as músicas criada com sucesso.")
print("Dimensões da matriz de Hellinger (N x N, onde N é o número de músicas):")
print(dim(matriz_dist_hellinger))

# Exibir uma pequena amostra da matriz de Hellinger (as primeiras 5x5 células)
if (nrow(matriz_dist_hellinger) > 0 && ncol(matriz_dist_hellinger) > 0) {
  print("\nAmostra da matriz de Distância de Hellinger (as primeiras 5x5 células):")
  print(matriz_dist_hellinger[1:min(5, nrow(matriz_dist_hellinger)), 1:min(5, ncol(matriz_dist_hellinger))])
}

# Opcional: Salvar a matriz de Hellinger para uso futuro
#saveRDS(matriz_dist_hellinger, file = "matriz_dist_hellinger_total.rds")


# --- 2. Calcular a Matriz de Distância de Jensen-Shannon (JSD) ---
# JSD tem intervalo [0, 1] se o logaritmo for na base 2, que é o padrão da philentropy.

print("\nCalculando a Matriz de Distância de Jensen-Shannon (JSD) (0 a 1)...")
matriz_dist_jsd <- distance(x = dtm_tf_normalizada_pronta, method = "jensen-shannon")

# Atribuir os nomes das músicas
row.names(matriz_dist_jsd) <- row.names(dtm_tf_normalizada_pronta)
colnames(matriz_dist_jsd) <- row.names(dtm_tf_normalizada_pronta)

print("Matriz de Distância de Jensen-Shannon (JSD) para todas as músicas criada com sucesso.")
print("Dimensões da matriz de JSD (N x N, onde N é o número de músicas):")
print(dim(matriz_dist_jsd))

# Exibir uma pequena amostra da matriz de JSD (as primeiras 5x5 células)
if (nrow(matriz_dist_jsd) > 0 && ncol(matriz_dist_jsd) > 0) {
  print("\nAmostra da matriz de Distância de Jensen-Shannon (JSD) (as primeiras 5x5 células):")
  print(matriz_dist_jsd[1:min(5, nrow(matriz_dist_jsd)), 1:min(5, ncol(matriz_dist_jsd))])
}

# Opcional: Salvar a matriz de JSD para uso futuro
#saveRDS(matriz_dist_jsd, file = "matriz_dist_jsd_total.rds")


# --- Exemplo de Acesso a um Par Específico ---
# Usando os mesmos códigos de música para testar a distância
musica_1_codigo <- "1" # Certifique-se que estes códigos existem nas linhas da matriz.
musica_2_codigo <- "1002" # Se estes códigos foram removidos por não ter palavras, o erro persistirá.

if (musica_1_codigo %in% row.names(matriz_dist_hellinger) &&
    musica_2_codigo %in% row.names(matriz_dist_hellinger)) {
  
  dist_hellinger_par <- matriz_dist_hellinger[musica_1_codigo, musica_2_codigo]
  dist_jsd_par <- matriz_dist_jsd[musica_1_codigo, musica_2_codigo]
  
  print(paste0(
    "\nA Distância de Hellinger entre a música ", musica_1_codigo,
    " e a música ", musica_2_codigo, " é: ", dist_hellinger_par
  ))
  
  print(paste0(
    "A Distância de Jensen-Shannon (JSD) entre a música ", musica_1_codigo,
    " e a música ", musica_2_codigo, " é: ", dist_jsd_par
  ))
  
} else {
  print("\nErro: Um ou ambos os códigos de música (para teste) não foram encontrados nas matrizes de distância (provavelmente foram removidos por não ter palavras).")
  print("Músicas presentes na matriz de distância (primeiras 10):")
  print(head(row.names(matriz_dist_hellinger), 10))
}
