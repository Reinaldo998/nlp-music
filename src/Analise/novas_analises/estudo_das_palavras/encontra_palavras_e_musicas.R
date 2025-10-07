library(dplyr)
library(stringr)

# --- Suponha que os seguintes objetos estejam disponíveis na sua sessão ---
# - letras_anotadas_df (o dataframe completo de anotações do udpipe)
# - data_final_pt (o seu dataframe de base original com Nome.x, Artista.x, Letra e ID)


lemmas_estranhos <- c("$2fe", "eole", "escor", "escar", "estrado","famíler","hor","ier","loucuro","mágom",
                        "noitir","oler","ouver","refr","roso","ruo","sangar","sentu","sertar","singuir","sombrar",
                        "ten","thar","tristezo","tristir","var","zer" ,"vom" )


# --- 1. Defina o Termo Lematizado para Rastreamento ---
palavra_lematizada_alvo <-   "morrar" 

ocorrencias_do_lemma <- letras_anotadas_df %>%
  filter(lemma == palavra_lematizada_alvo)

codigos_com_lemma <- unique(ocorrencias_do_lemma$doc_id)

tokens_originais <- unique(ocorrencias_do_lemma$token)
print(tokens_originais)



# --- 3. Juntar com os Metadados Originais ---

# Filtra o dataframe principal para pegar apenas as músicas que contêm o lemma
df_musicas_encontradas <- data_final_pt %>%
  filter(ID %in% codigos_com_lemma) %>%
  # Renomeia as colunas para clareza no output
  select(
    ID_Musica = ID,
    Nome_Musica = Nome.x,
    Artista = Artista.x,
    Ano,
    Letra_Original = Letra
  )

# --- 4. Exibir o Resultado ---

cat(paste0("Total de músicas encontradas: ", nrow(df_musicas_encontradas), "\n"))
cat("------------------------------------------------------------------------\n")

# Para visualização das músicas (metadados)
print("Metadados das músicas que contêm o lemma:")
View(df_musicas_encontradas)
print(dim(df_musicas_encontradas))

#===============================================================================

ids_para_inspecao <- c(7622) 

linhas_selecionadas <- letras_df %>%
  filter(doc_id %in% ids_para_inspecao)

View(letras_df)
print(linhas_selecionadas)


