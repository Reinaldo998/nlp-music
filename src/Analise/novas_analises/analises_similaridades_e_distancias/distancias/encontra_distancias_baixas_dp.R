# Certifique-se de que 'df_distancias_jsd_completo' e 'data_dp' estão carregados na sua sessão

# CONVERSÃO DE TIPO: Garante que a coluna ID em data_dp é do tipo CHARACTER
data_dp$ID <- as.character(data_dp$ID)

# Defina o ID do tópico que você quer analisar
topico_para_analisar <- 21 

cat(paste0("\n--- Identificando Outliers de Distância JSD para o Tópico ", topico_para_analisar, " ---\n"))

df_distancias_internas_topico <- df_distancias_jsd_completo %>%
  filter(topico_1 == topico_para_analisar, topico_2 == topico_para_analisar)

if (nrow(df_distancias_internas_topico) < 2) {
  cat(paste0("Aviso: Tópico ", topico_para_analisar, " tem menos de 2 pares de músicas. Não é possível calcular outliers.\n"))
} else {
  boxplot_stats_topico <- boxplot.stats(df_distancias_internas_topico$distancia)
  outliers_valores <- boxplot_stats_topico$out
  
  pares_musicas_outliers <- df_distancias_internas_topico %>%
    filter(distancia %in% outliers_valores) %>%
    arrange(desc(distancia))
  
  if (nrow(pares_musicas_outliers) > 0) {
    cat(paste0("Pares de Músicas Outliers no Tópico ", topico_para_analisar, ":\n"))
    print(pares_musicas_outliers)
    
    detalhes_outliers <- pares_musicas_outliers %>%
      left_join(data_dp %>% select(ID, Artista.y, Nome.y, Letra), by = c("ID_musica_1" = "ID")) %>%
      rename(Artista_1 = Artista.y, Nome_Musica_1 = Nome.y, Letra_1 = Letra) %>%
      left_join(data_dp %>% select(ID, Artista.y, Nome.y, Letra), by = c("ID_musica_2" = "ID")) %>%
      rename(Artista_2 = Artista.y, Nome_Musica_2 = Nome.y, Letra_2 = Letra) %>%
      select(ID_musica_1, Artista_1, Nome_Musica_1, Letra_1, 
             ID_musica_2, Artista_2, Nome_Musica_2, Letra_2, distancia) %>%
      arrange(desc(distancia))
    
    cat("\n--- Detalhes dos Pares de Músicas Outliers (incluindo Letras) ---\n")
    print(detalhes_outliers)
    
  } else {
    cat(paste0("Não foram encontrados outliers significativos no Tópico ", topico_para_analisar, ".\n"))
  }
}

View(detalhes_outliers)
