
library(ggplot2)
library(dplyr)

###########################################################################################
######################## Número total de palavras #############

dados_media <- Mqtdade %>%
  group_by(Ano) %>%
  summarize(media_palavras = mean(N_palavras, na.rm = TRUE))


ggplot(dados_media, aes(x = Ano, y = media_palavras)) +
  geom_line(color = "steelblue", size = 1) + 
  geom_point(color = "red", size = 2) +  
  labs(title = "Média do Número de Palavras por Ano", x = "Ano", y = "Média de Palavras") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dados_media, aes(x = Ano, y = media_palavras)) +
  geom_line(color = "steelblue", size = 1) +  
  geom_point(color = "red", size = 2) +  
  labs(title = "Média do Número de Palavras por Ano", x = "Ano", y = "Média de Palavras") +
  scale_x_continuous(breaks = seq(min(dados_media$Ano), max(dados_media$Ano), by = 10)) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####################################################################################################################
############# Palavras binárias ##############

dados_media2 <- Mqtdade %>%
  group_by(Ano) %>%
  summarize(media_palavras = mean(N_palavras_bin, na.rm = TRUE))

ggplot(dados_media2, aes(x = Ano, y = media_palavras)) +
  geom_line(color = "steelblue", size = 1) +  
  geom_point(color = "red", size = 2) +  
  labs(title = "Média do Número de Palavras por Ano", x = "Ano", y = "Média de Palavras") +
  scale_x_continuous(breaks = seq(min(dados_media2$Ano), max(dados_media2$Ano), by = 10)) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#############################################################################################
####### Em portugues #############

dados_portugues <- Mqtdade %>%
  filter(Idioma_1 == "pt") 


dados_media_portugues <- dados_portugues %>%
  group_by(Ano) %>%
  summarize(media_palavras = mean(N_palavras_bin, na.rm = TRUE))


ggplot(dados_media_portugues, aes(x = Ano, y = media_palavras)) +
  geom_line(color = "steelblue", size = 1) +  
  geom_point(color = "red", size = 2) +  
  labs(title = "Média do Número de Palavras por Ano (Músicas em Português)", x = "Ano", y = "Média de Palavras") +
  scale_x_continuous(breaks = seq(min(dados_media_portugues$Ano), max(dados_media_portugues$Ano), by = 10)) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#############################################################################################################
###################### Proporção de idiomas na base #############

proporcao_idiomas <- Mqtdade %>%
  group_by(Idioma_2) %>%
  summarize(contagem = n()) %>%  
  mutate(proporcao = contagem / sum(contagem))  ma

proporcao_idiomas
View(proporcao_idiomas)

############################################################################################################
######################### Top artista da base #################
top_50_artistas <- Mqtdade %>%
  count(Artista) %>%  
  arrange(desc(n)) %>%  
  head(10)  

xtable(top_50_artistas)
length(table(Mqtdade$Artista))
##########################################################################################################
################# Top artistas por ano ######################
Mqtdade <- Mqtdade %>%
  mutate(Decada = floor(Ano / 10) * 10)  

top_artistas_decada <- Mqtdade %>%
  group_by(Decada, Artista) %>%
  count() %>%  
  arrange(Decada, desc(n)) %>%  
  group_by(Decada) %>%
  slice_head(n = 10)  

View(top_artistas_decada)

#################################################################################################################
################### Nuvem de plavras por década ##################
corpus_df <- data.frame(Letra = textos, Idioma = Mqtdade$Idioma_1, Ano = data_final$Ano)


dados_portugues <- corpus_df %>%
  filter(Idioma == "pt")  


dados_portugues <- dados_portugues %>%
  mutate(Decada = floor(Ano / 10) * 10)  


corpus_decada <- dados_portugues %>%
  group_by(Decada) %>%
  summarise(texto = paste(Letra, collapse = " ")) %>%
  ungroup()  


for(i in unique(corpus_decada$Decada)) {
  musica_decada <- corpus_decada %>%
    filter(Decada == i)
  
  corpus <- Corpus(VectorSource(musica_decada$texto))

  corpus_clean <- corpus %>%
    tm_map(tolower) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("pt")) %>%
    tm_map(stripWhitespace)
  
  
  wordcloud(
    corpus_clean, 
    min.freq = 5,    
    scale = c(3,0.5), 
    colors = brewer.pal(8, "Dark2"),
    random.order = FALSE,
    main = paste("Nuvem de Palavras - Década de", i)
  )
}

##################################################################################################
############# Palavras mais frequentes por decada ##############
dados_portugues <- data_final %>%
  filter(idiomas == "pt")


dados_portugues <- dados_portugues %>%
  mutate(Decada = paste0(substr(Ano, 1, 3), "0"))


top5_artistas_por_decada <- dados_portugues %>%
  group_by(Decada, Artista.x) %>%
  summarise(Frequencia = n(), .groups = "drop") %>%
  arrange(Decada, desc(Frequencia)) %>%
  group_by(Decada) %>%
  slice_head(n = 5) %>%
  ungroup() %>%

  mutate(Posicao = row_number()) %>%

  tidyr::pivot_wider(names_from = Decada, values_from = Artista.x, values_fn = list) %>%
  
  arrange(Posicao) %>%
  select(-Posicao)

View(top5_artistas_por_decada)


