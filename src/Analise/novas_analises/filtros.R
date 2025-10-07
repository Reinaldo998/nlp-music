# Suponha que você queira filtrar para o grupo de ID 5
id_do_grupo <- 5

# Filtra o dataframe 'data_dp' para manter apenas as linhas onde 'Grupos_MM_DP' é igual a 5
musicas_do_grupo_5 <- data_dp %>%
  filter(Grupos_MM_DP == id_do_grupo)

# Opcional: Para visualizar o resultado
# View(musicas_do_grupo_5)

View(data_dp %>%
       filter(Grupos_MM_DP == 2))


View(data_dp %>%
       filter(Ano == 2016)%>%select(ID))

# Suponha que você queira filtrar para os IDs 9595, 1495 e 6073
vetor_de_ids <- c(1032, 1127, 6073)

# Garante que a coluna 'ID' em data_dp é do mesmo tipo que o vetor de IDs
data_dp$ID <- as.character(data_dp$ID)
vetor_de_ids <- as.character(vetor_de_ids)

# Filtra o dataframe 'data_dp' para manter apenas as linhas onde o 'ID' está no vetor
musicas_dos_ids <- data_dp %>%
  filter(ID %in% vetor_de_ids)

# Opcional: Para visualizar o resultado
# View(musicas_dos_ids)

View(data_dp %>%
       filter(ID %in% ids_musicas_unicas))
library(dplyr)

# =================================================================================
# 1. Identificar e filtrar as músicas que estão em grupos de apenas um membro
# =================================================================================

# Primeiro, contamos o número de músicas em cada grupo.
# Em seguida, filtramos para manter apenas os IDs dos grupos com 1 música.
grupos_com_uma_musica <- data_dp %>%
  count(Grupos_MM_DP) %>%
  filter(n == 1) %>%
  pull(Grupos_MM_DP) # Extrai apenas os IDs dos grupos

# Agora, filtramos o dataframe original (data_dp) para manter apenas
# as linhas onde o grupo está na lista de grupos com uma música.
musicas_em_grupos_unicos <- data_dp %>%
  filter(Grupos_MM_DP %in% grupos_com_uma_musica)

cat("DataFrame 'musicas_em_grupos_unicos' criado. Total de músicas encontradas:", nrow(musicas_em_grupos_unicos), "\n")

View(musicas_em_grupos_unicos)

# =================================================================================
# 2. Salvar os IDs dessas músicas em um vetor
# =================================================================================

# Extrai a coluna 'ID' do novo dataframe e salva em um vetor.
ids_musicas_unicas2 <- musicas_em_grupos_unicos$ID

cat("IDs das músicas em grupos únicos salvos no vetor 'ids_musicas_unicas'.\n")







#=======================================================================

View(data_dp %>%
       filter(Grupos_MM_DP == 137))

View(data_com_topicos_atual%>%
       filter(topico_lda == 25))

#=======================================================================

# --- Filtro: Encontrar IDs de Músicas de um Tópico Específico ---
# Suponha que você queira os IDs das músicas do Tópico 5.
id_do_topico <- 5

# Filtra o dataframe 'data_dp' para o tópico desejado e extrai a coluna 'ID'.
# O resultado será um vetor de IDs de músicas.
ids_musicas_do_topico <- data_dp %>%
  dplyr::filter(Grupos_MM_DP == id_do_topico) %>%
  dplyr::pull(ID)

# Imprime o vetor de IDs para visualização
cat(paste0("Total de músicas encontradas no Tópico ", id_do_topico, ": ", length(ids_musicas_do_topico), "\n"))
print(head(ids_musicas_do_topico))
#=========================================================================================================

# --- Filtro: Encontrar Palavras de uma Música Específica ---
# Suponha que você queira as palavras da música com o ID "5988".
id_da_musica <- "8014"

# 1. Garante que o ID da música seja do mesmo tipo (caractere) que os nomes das linhas na DTM.
id_da_musica <- as.character(id_da_musica)

# 2. Filtra a matriz 'dtm_filtrada' para a linha da música específica.
# O resultado é um vetor de contagens de palavras para aquela música.
#palavras_da_musica <- dtm_filtrada[id_da_musica, ]
palavras_da_musica <- as.matrix(dtm_filtrada[id_da_musica, ])


# 3. Encontra as palavras que têm contagem maior que zero.
# O which() retorna os índices, e os nomes das colunas correspondentes são as palavras.
palavras_presentes <- colnames(palavras_da_musica)[which(palavras_da_musica > 0)]

# Imprime o vetor de palavras
cat(paste0("Palavras encontradas para a música com ID ", id_da_musica, ":\n"))
print(palavras_presentes)

tabela_top_palavras_alinhada$`Tópico 7`
#tabela_top_palavras_alinhada$`Tópico 2`
#tabela_top_palavras_alinhada$`Tópico 9`

