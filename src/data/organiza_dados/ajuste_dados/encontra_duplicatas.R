# Carregar os pacotes necessários
library(dplyr)
library(tm)
library(SnowballC)

# Certifique-se de que df_final_para_analise_total está carregado
# e que a coluna Letra_Site existe e contém as letras das músicas.

df_final_total_combinado_simples<-df_analise_total_combinado_simples
# --- Identificar TODAS as Duplicatas com base em Nome_Site ---

# 1. Criar o dataframe de músicas únicas (que será usado para a DTM)
# (Mantém apenas a primeira ocorrência de cada Nome_Site)
df_final_para_analise_total_unique <- df_final_total_combinado_simples %>%
  distinct(Nome_Site, .keep_all = TRUE)

# 2. Identificar e guardar TODAS as ocorrências de músicas com Nome_Site duplicado
# (Inclui a primeira ocorrência que será mantida e as subsequentes que serão removidas)
df_todas_as_duplicatas <- df_final_total_combinado_simples %>%
  group_by(Nome_Site) %>%
  filter(n() > 1) %>% # Filtra grupos onde o Nome_Site aparece mais de uma vez
  ungroup() # Desagrupar o dataframe após a operação

# --- NOVO MÉTODO PARA ORDENAR df_todas_as_duplicatas (SEM O MUTATE EXPLÍCITO) ---
if (nrow(df_todas_as_duplicatas) > 0) { # Verifica se há duplicatas para ordenar
  df_todas_as_duplicatas <- df_todas_as_duplicatas %>%
    # Ordenar diretamente: por Nome_Site, depois por se o Codigo está no set de únicos (TRUE primeiro), e por Codigo para desempate
    arrange(Nome_Site, desc(Codigo %in% df_final_para_analise_total_unique$Codigo), Codigo)
}


print("Todas as Músicas com Nome_Site Duplicado (df_todas_as_duplicatas):")
View(df_todas_as_duplicatas)
print(paste0("Total de ocorrências de Nome_Site duplicados (incluindo as mantidas): ", nrow(df_todas_as_duplicatas)))


print("Músicas Únicas para Análise (df_final_para_analise_total_unique):")
View(df_final_para_analise_total_unique)
print(paste0("Total de músicas ÚNICAS após remoção de duplicatas: ", nrow(df_final_para_analise_total_unique)))
print(paste0("Total de músicas ANTES da remoção de duplicatas: ", nrow(df_final_para_analise_total)))
#=================================================================================================================================================
# Voltar músicas importantes #

library(dplyr)
library(purrr) # Para bind_rows

# Certifique-se de que os seguintes dataframes estão carregados em seu ambiente R:
# - df_final_para_analise_total_unique (o dataframe atual de músicas únicas)
# - df_todas_as_duplicatas (o dataframe com todas as ocorrências de músicas duplicadas)
# - df_final_total_combinado_simples (o dataframe original que contém todas as músicas antes da desduplicação)


# --- 1. Defina o vetor com os Códigos das músicas que você quer REINCLUIR ---
# ATENÇÃO: Substitua os exemplos abaixo pelos CÓDIGOS REAIS das suas músicas que foram removidas como duplicatas.
codigos_para_reincluir <- c(1077, 1776,143,1157,8490,762,1495,140,6792,7581,8087,6666,1577,1450,8594,7366,8090,310,9967,
                            6608,6660,1221,85100,9327,204,8918,9274,9818,6998,1286,433,7689,9545,72,7014,9274,
                            820,447,1299,5988,6213,282,414,1652,6112,8879,9901,881,9231,683,149,8855,8856,9327) # Exemplo: Códigos que eram duplicatas e agora devem voltar

print(paste0("Número de Códigos a serem reincluídos: ", length(codigos_para_reincluir)))


# --- 2. Localize as músicas a serem reincluídas ---
# Vamos pegá-las do df_final_total_combinado_simples para garantir que temos a versão completa delas.
musicas_a_reincluir <- df_final_total_combinado_simples %>%
  filter(Codigo %in% codigos_para_reincluir)

print("Músicas a serem reincluídas:")
View(musicas_a_reincluir)
print(dim(musicas_a_reincluir))


# --- 3. Combine o dataframe de únicas com as músicas a serem reincluídas ---
# df_final_para_analise_total_unique será o novo dataframe atualizado.
# bind_rows empilha. distinct garante que cada Codigo seja único, preferindo a primeira ocorrência.
# Isso significa que, se um Código de 'musicas_a_reincluir' já estiver em
# 'df_final_para_analise_total_unique', a versão que já está no 'unique' será mantida.
# Se o Código de 'musicas_a_reincluir' for uma duplicata que foi removida, ele será adicionado de volta.
df_final_para_analise_total_unique <- bind_rows(
  df_final_para_analise_total_unique, # O set de únicos atual
  musicas_a_reincluir                   # As músicas que queremos reincluir
) %>%
  distinct(Codigo, .keep_all = TRUE) # Remove quaisquer duplicatas resultantes da combinação, mantendo a primeira ocorrência


# --- 4. Verifique o novo dataframe de músicas únicas ---
print("Novo Dataframe de Músicas Únicas (após reinclusão):")
View(df_final_para_analise_total_unique)
print(paste0("Novo total de músicas únicas: ", nrow(df_final_para_analise_total_unique)))

# Opcional: Você pode querer verificar quais Nomes_Site agora são duplicados
# apos essa reinclusão, se isso for uma preocupação para a sua DTM.
# df_final_para_analise_total_unique %>%
#   group_by(Nome_Site) %>%
#   filter(n() > 1) %>%
#   ungroup() %>%
#   View()
