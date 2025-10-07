library(dplyr)
library(tibble) # Para rownames_to_column

library(dplyr)
library(tibble)

# --- Supondo que 'dtm_letras_tf' e 'data_dp_novo' estejam na memória ---
# dtm_letras_tf: Sua matriz de frequência de termos (DocumentTermMatrix)
# data_dp_novo: Seu dataframe original com as colunas 'ID' e 'Ano'

# --- 1. Criar a Matriz Binária a partir da DTM de Frequência (TF) ---
# O objetivo é ter 1 (presente) e 0 (ausente)
dtm_matrix_binary <- as.matrix(dtm_letras_tf)
dtm_matrix_binary[dtm_matrix_binary > 0] <- 1

# --- 2. Preparar o Mapa de ID para Ano ---
# Cria um dataframe simples para a junção
mapa_id_ano <- data_dp%>%
  # Seleciona as colunas essenciais
  select(ID, Ano) %>%
  # Garante que o ID é do mesmo tipo que o rownames da DTM para evitar erros
  mutate(ID = as.character(ID))


# --- 3. Juntar a Matriz Binária com os Anos para Criar 'dtm_com_ano' ---

dtm_com_ano <- dtm_matrix_binary %>%
  # Converte a matriz para um dataframe
  as.data.frame() %>%
  # Transforma os nomes das linhas (que são os IDs) em uma coluna chamada "ID"
  tibble::rownames_to_column("ID") %>%
  # Junta a DTM (agora um dataframe) com o mapa de ID/Ano
  left_join(mapa_id_ano, by = "ID")

# Verificação:
cat("\n--- Verificação de 'dtm_com_ano' ---\n")
print(head(dtm_com_ano[, c(1, (ncol(dtm_com_ano)-5):ncol(dtm_com_ano))], 5))

# --- 1. Definir o ano que você quer analisar ---
ano_alvo <- 1967 # Substitua pelo ano desejado


# --- 2. Preparar os dados para a análise ---

# Filtra o dataframe com a DTM para o ano alvo
dtm_ano_alvo <- dtm_com_ano %>%
  filter(Ano == ano_alvo)


# Remove as colunas de metadados ('ID' e 'Ano') para trabalhar apenas com as palavras
dtm_ano_alvo_somente_palavras <- dtm_ano_alvo %>%
  select(-ID, -Ano)


# --- 3. Encontrar as palavras que aparecem no ano alvo ---

# colSums() soma os valores de cada coluna
# A condição colSums(...) > 0 retorna TRUE para as colunas com pelo menos uma ocorrência
palavras_que_apareceram_no_ano <- colnames(dtm_ano_alvo_somente_palavras)[colSums(dtm_ano_alvo_somente_palavras) > 0]


# --- 4. Exibir o resultado ---
cat(paste0("Total de palavras únicas que apareceram em ", ano_alvo, ": ", length(palavras_que_apareceram_no_ano), "\n"))
cat("As 20 primeiras palavras que apareceram são:\n")
print(head(palavras_que_apareceram_no_ano, 20))


# Opcional: Para ver a lista completa de palavras, remova o head()
# View(palavras_que_apareceram_no_ano)
#palavras_2016<-palavras_que_apareceram_no_ano
#palavras_1993<-palavras_que_apareceram_no_ano
#palavras_1967<-palavras_que_apareceram_no_ano

print(palavras_2016)#89
print(palavras_1993)#54
print(palavras_1967)#53

length(palavras_2016)
length(palavras_1993)
length(palavras_1967)


View(df_musicas_por_ano)
