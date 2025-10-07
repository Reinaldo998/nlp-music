# Certifique-se de que 'df_distancias_jsd_completo' e 'data_dp' estão carregados na sua sessão

# CONVERSÃO DE TIPO: Garante que a coluna ID em data_dp é do tipo CHARACTER
# Isso é crucial para que o join funcione corretamente com ID_musica_1/2
data_dp$ID <- as.character(data_dp$ID)

cat("\n--- Identificando os 50 Pares de Músicas com as Menores Distâncias JSD Internas ---\n")

# 1. Filtrar apenas as distâncias internas (topico_1 == topico_2)
# 2. Ordenar pela distância em ordem crescente (do menor para o maior)
# 3. Selecionar os 50 primeiros pares
pares_menores_distancias_jsd <- df_distancias_jsd_completo %>%
  filter(topico_1 == topico_2) %>% # Filtra apenas distâncias internas
  arrange(distancia) %>% # Ordena da menor para a maior distância
  head(50) # Pega os 50 primeiros pares

# 4. Opcional: Juntar com o dataframe original para obter mais detalhes das músicas e letras
# Usando as colunas Artista.y, Nome.y (para nome da música) e Letra de data_dp
detalhes_menores_distancias <- pares_menores_distancias_jsd %>%
  left_join(data_dp %>% select(ID, Artista.y, Nome.y, Letra), by = c("ID_musica_1" = "ID")) %>%
  rename(Artista_1 = Artista.y, Nome_Musica_1 = Nome.y, Letra_1 = Letra) %>%
  left_join(data_dp %>% select(ID, Artista.y, Nome.y, Letra), by = c("ID_musica_2" = "ID")) %>%
  rename(Artista_2 = Artista.y, Nome_Musica_2 = Nome.y, Letra_2 = Letra) %>%
  select(topico_1, ID_musica_1, Artista_1, Nome_Musica_1, Letra_1,
         ID_musica_2, Artista_2, Nome_Musica_2, Letra_2, distancia) %>%
  arrange(distancia) # Garante que a ordenação final seja pela distância

cat("\n--- Detalhes dos 50 Pares de Músicas com as Menores Distâncias JSD Internas (incluindo Letras) ---\n")
print(detalhes_menores_distancias)

View(detalhes_menores_distancias)

