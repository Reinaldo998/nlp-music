#===================================================================
# Carregamento de Pacotes
#===================================================================
library(dplyr)
library(Matrix) # Para a função Matrix() caso a dtm seja um objeto Matrix::sparseMatrix
library(ggplot2)
library(scales)

dim(dtm_letras_tf)
dim(data_dp)
#
#===================================================================
# Assumindo que os dataframes e matrizes necessários já estão na memória:
# - dtm (matriz documento-termo com todas as palavras, contagem por frequência absoluta)
# - data_dp (dataframe com todas as informações das músicas)
#===================================================================

cat("Iniciando o cálculo do número de palavras totais e únicas por música...\n")

# --- 1. Calcular o número de palavras totais (ocorrências) por música ---
# Garante que a dtm é uma matriz para usar a função rowSums
dtm_matrix <- as.matrix(dtm_letras_tf)
num_palavras_total_por_musica <- rowSums(dtm_matrix)

# --- 2. Calcular o número de palavras únicas por música ---
# Cria uma matriz binária onde 1 indica a presença da palavra e 0 a ausência
dtm_matrix_binary <- (dtm_matrix > 0) * 1
num_palavras_unicas_por_musica <- rowSums(dtm_matrix_binary)

# --- 3. Criar um dataframe com as novas métricas ---
# O ID da música é o nome da linha da DTM
df_novas_metricas <- data.frame(
  ID_musica = as.numeric(row.names(dtm_matrix)),
  Total_Palavras = num_palavras_total_por_musica,
  Palavras_Unicas = num_palavras_unicas_por_musica
)

# --- 4. Adicionar as novas colunas ao dataframe principal (data_dp) ---
# Usa left_join para manter todas as linhas do data_dp
# e adicionar as novas colunas com base no ID da música
data_dp_novo <- data_dp %>%
  left_join(df_novas_metricas, by = c("ID" = "ID_musica"))

cat("\nAnálise concluída. Duas novas colunas, 'Total_Palavras' e 'Palavras_Unicas', foram adicionadas ao dataframe 'data_dp_novo'.\n")

print("\n--- Amostra do novo dataframe com as colunas adicionadas ---\n")
print(head(data_dp_novo))

#=================================================================================

#
#===================================================================
# Assumindo que o dataframe 'data_dp_novo' já está na memória com as colunas:
# "Nome.x", "Artista.x", "Ano", "ID", "Nome.y", "Artista.y", "Letra", "Idioma",
# "Grupos_MM_DP", "Total_Palavras", "Palavras_Unicas"
#===================================================================

cat("Iniciando a análise e geração de gráficos de palavras por ano...\n")

# --- 1. Calcular o número médio de palavras totais por ano ---
media_total_palavras_por_ano <- data_dp_novo %>%
  group_by(Ano) %>%
  summarise(media_palavras = mean(Total_Palavras, na.rm = TRUE),
            .groups = "drop")

# --- 2. Gerar o gráfico de linhas para o número total de palavras ---
cat("\n\n========================================================\n")
cat(" GRÁFICO: NÚMERO MÉDIO DE PALAVRAS TOTAIS POR ANO\n")
cat("========================================================\n")

p_total <- ggplot(media_total_palavras_por_ano, aes(x = Ano, y = media_palavras)) +
  geom_line(linewidth = 1, color = "darkblue") +
  geom_point(size = 2, color = "darkblue") +
  labs(
    title = "Número Médio de Palavras Totais por Música ao Longo dos Anos",
    x = "Ano",
    y = "Número Médio de Palavras Totais"
  ) +
  scale_y_continuous(labels = scales::comma) + # Formata o eixo Y com vírgulas para números grandes
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(p_total)


# --- 3. Calcular o número médio de palavras únicas por ano ---
media_palavras_unicas_por_ano <- data_dp_novo %>%
  group_by(Ano) %>%
  summarise(media_palavras = mean(Palavras_Unicas, na.rm = TRUE),
            .groups = "drop")

# --- 4. Gerar o gráfico de linhas para o número de palavras únicas ---
cat("\n\n========================================================\n")
cat(" GRÁFICO: NÚMERO MÉDIO DE PALAVRAS ÚNICAS POR ANO\n")
cat("========================================================\n")

p_unicas <- ggplot(media_palavras_unicas_por_ano, aes(x = Ano, y = media_palavras)) +
  geom_line(linewidth = 1, color = "darkgreen") +
  geom_point(size = 2, color = "darkgreen") +
  labs(
    title = "Número Médio de Palavras Únicas por Música ao Longo dos Anos",
    x = "Ano",
    y = "Número Médio de Palavras Únicas"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(p_unicas)

#===============================================================================
cat("\nAnálise e geração dos dois gráficos de linhas concluídas.\n")

# --- Calcular o total de palavras ---
total_palavras_na_base <- sum(data_dp_novo$Total_Palavras, na.rm = TRUE)

# --- Calcular o total de palavras únicas ---
total_palavras_unicas_na_base <- sum(data_dp_novo$Palavras_Unicas, na.rm = TRUE)

cat("\n--- Resumo de Palavras na Base de Dados ---\n")
cat(paste0("Número Total de Ocorrências de Palavras na base: ", total_palavras_na_base, "\n"))
cat(paste0("Número Total de Palavras Únicas na base: ",  total_palavras_unicas_na_base, "\n"))
cat("--------------------------------------------\n")
#==============================================================================

# --- 2. Calcular o número total de palavras e palavras únicas por ano ---
soma_palavras_por_ano <- data_dp_novo %>%
  group_by(Ano) %>%
  summarise(
    soma_total_palavras = sum(Total_Palavras, na.rm = TRUE),
    soma_unicas_palavras = sum(Palavras_Unicas, na.rm = TRUE),
    .groups = "drop"
  )

# --- 3. Calcular a proporção de palavras totais e únicas por ano ---
proporcao_palavras_por_ano <- soma_palavras_por_ano %>%
  mutate(
    proporcao_total = soma_total_palavras / total_palavras_na_base,
    proporcao_unicas = soma_unicas_palavras / total_palavras_unicas_na_base
  )

# --- 4. Gerar o gráfico de linhas para a proporção de palavras totais ---
cat("\n\n===================================================================\n")
cat(" GRÁFICO: PROPORÇÃO TOTAL DE PALAVRAS POR ANO\n")
cat("===================================================================\n")

p_proporcao_total <- ggplot(proporcao_palavras_por_ano, aes(x = Ano, y = proporcao_total)) +
  geom_line(linewidth = 1, color = "darkblue") +
  geom_point(size = 2, color = "darkblue") +
  labs(
    title = "Proporção do Total de Palavras por Ano em Relação à Base Completa",
    x = "Ano",
    y = "Proporção de Ocorrências de Palavras"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(p_proporcao_total)


# --- 5. Gerar o gráfico de linhas para a proporção de palavras únicas ---
cat("\n\n===================================================================\n")
cat(" GRÁFICO: PROPORÇÃO DE PALAVRAS ÚNICAS POR ANO\n")
cat("===================================================================\n")

p_proporcao_unicas <- ggplot(proporcao_palavras_por_ano, aes(x = Ano, y = proporcao_unicas)) +
  geom_line(linewidth = 1, color = "darkgreen") +
  geom_point(size = 2, color = "darkgreen") +
  labs(
    title = "Proporção do Total de Palavras Únicas por Ano em Relação à Base Completa",
    x = "Ano",
    y = "Proporção de Palavras Únicas"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(p_proporcao_unicas)

cat("\nAnálise de proporção por ano concluída. Os gráficos foram gerados.\n")

#===============================================================================

soma_unicas_por_ano <- data_dp_novo %>%
  group_by(Ano) %>%
  summarise(soma_unicas = sum(Palavras_Unicas, na.rm = TRUE),
            .groups = "drop")

# --- 3. Calcular a proporção de palavras únicas por ano em relação ao vocabulário total ---
proporcao_unicas_por_ano_vs_vocab <- soma_unicas_por_ano %>%
  mutate(proporcao_unicas = soma_unicas / total_colunas_dtm_binaria)

total_colunas_dtm_binaria <- ncol(dtm_matrix_binary)

# --- 4. Gerar o gráfico de linhas para a proporção de palavras únicas ---
cat("\n\n====================================================================================\n")
cat(" GRÁFICO: PROPORÇÃO DE PALAVRAS ÚNICAS POR ANO (vs. Vocabulário Total da Base)\n")
cat("====================================================================================\n")

p_proporcao_unicas_vocab <- ggplot(proporcao_unicas_por_ano_vs_vocab, aes(x = Ano, y = proporcao_unicas)) +
  geom_line(linewidth = 1, color = "darkgreen") +
  geom_point(size = 2, color = "darkgreen") +
  labs(
    title = "Proporção de Palavras Únicas por Ano em Relação ao Vocabulário Completo",
    subtitle = paste("Tamanho do Vocabulário Total (Colunas da DTM):", total_colunas_dtm_binaria),
    x = "Ano",
    y = "Proporção de Palavras Únicas do Ano"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(p_proporcao_unicas_vocab)

cat("\nAnálise de proporção por ano concluída. O gráfico foi gerado.\n")


# =================================================================================

# --- DEFINE O ANO PARA FILTRAR ---
# Altere o valor da variável 'ano_para_filtrar' para o ano desejado.
ano_para_filtrar <- 2016

# Filtra o dataframe para o ano especificado e seleciona as colunas de interesse
df_palavras_por_ano <- data_dp_novo %>%
  filter(Ano == ano_para_filtrar) %>%
  select(ID, Nome.y,Artista.y,Letra,Grupos_MM_DP,Total_Palavras,Palavras_Unicas) %>% # Seleciona as colunas de ID, nome da música e total de palavras
  arrange(desc(Total_Palavras)) # Opcional: ordena as músicas pela contagem de palavras

# Exibe o resultado da filtragem
cat(paste0("Músicas do ano ", ano_para_filtrar, " com a contagem de palavras:\n"))
View(df_palavras_por_ano)

# =================================================================================
# ASSUMINDO que o dataframe 'data_dp' já está carregado
# =================================================================================

cat("Iniciando o cálculo e a geração do gráfico de músicas por ano...\n")

# --- 1. Calcular o número de músicas por ano ---
# Agrupa os dados por 'Ano' e conta o número de músicas em cada ano.
df_musicas_por_ano <- data_dp %>%
  group_by(Ano) %>%
  summarise(
    numero_de_musicas = n(),
    .groups = "drop"
  )

# --- 2. Gerar o gráfico de linhas ---
cat("\n\n========================================================\n")
cat(" GRÁFICO: NÚMERO DE MÚSICAS POR ANO\n")
cat("========================================================\n")

p_musicas_por_ano <- ggplot(df_musicas_por_ano, aes(x = Ano, y = numero_de_musicas)) +
  geom_line(linewidth = 1, color = "darkblue") +
  geom_point(size = 2, color = "darkblue") +
  labs(
    title = "Número de Músicas por Ano",
    x = "Ano",
    y = "Número de Músicas"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_musicas_por_ano)

cat("\nGráfico de linhas gerado com sucesso. Ele mostra o número de músicas por ano.\n")

# =================================================================================
# ASSUMINDO que os dataframes 'data_dp_novo' e a matriz 'dtm_matrix_binary'
# já estão carregados na memória.
# =================================================================================

cat("Iniciando o cálculo da proporção de palavras únicas por ano...\n")

total_colunas_dtm_binary <- ncol(dtm_matrix_binary)

# Otimização para calcular a soma de palavras únicas por ano:
# Primeiro, crie um dataframe mapeando o ID da música ao ano
mapa_id_ano <- data_dp_novo %>%
  select(ID, Ano) %>%
  mutate(ID = as.character(ID))



# Agora, junte os IDs da matriz binária com os anos para agrupar
dtm_com_ano <- dtm_matrix_binary %>%
  as.data.frame() %>%
  tibble::rownames_to_column("ID") %>%
  left_join(mapa_id_ano, by = "ID")


# Soma o total de palavras únicas por ano, agora que os dados estão mapeados
# Supondo que dtm_com_ano está carregado
soma_unicas_por_ano <- dtm_com_ano %>%
  # removemos o select(-ID) do pipeline, e group_by() garante que Ano seja mantido.
  group_by(Ano) %>%
  summarise(
    # A correção está aqui: pick() agora opera em todas as colunas de palavras,
    # removendo apenas o ID.
    soma_unicas = sum(colSums(pick(-ID)) > 0),
    .groups = "drop"
  )
# --- 2. Calcular a proporção de palavras únicas por ano em relação ao vocabulário total ---
proporcao_unicas_por_ano_vs_vocab <- soma_unicas_por_ano %>%
  mutate(proporcao_unicas = soma_unicas / total_colunas_dtm_binary)

# --- 3. Gerar o gráfico de linhas para a proporção de palavras únicas ---
cat("\n\n====================================================================================\n")
cat(" GRÁFICO: PROPORÇÃO DE PALAVRAS ÚNICAS POR ANO (vs. Vocabulário Total da Base)\n")
cat("====================================================================================\n")

p_proporcao_unicas_vocab <- ggplot(proporcao_unicas_por_ano_vs_vocab, aes(x = Ano, y = proporcao_unicas)) +
  geom_line(linewidth = 1, color = "darkgreen") +
  geom_point(size = 2, color = "darkgreen") +
  labs(
    title = "Proporção de Palavras Únicas por Ano em Relação ao Vocabulário Completo",
    subtitle = paste("Tamanho do Vocabulário Total (Colunas da DTM):", total_colunas_dtm_binary),
    x = "Ano",
    y = "Proporção de Palavras Únicas do Ano"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_proporcao_unicas_vocab)

cat("\nAnálise de proporção por ano concluída. O gráfico foi gerado.\n")

