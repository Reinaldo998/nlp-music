library(dplyr)

# --- 1. Definir o Caminho de Base ---
# Seu diretório de trabalho é '/home/reinaldo'.
# A pasta dos arquivos está em uma subpasta 'Reinaldo/HDP' dentro dele.
# O file.path() irá lidar com as barras de forma correta.
BASE_PATH <- file.path("/home/reinaldo", "Reinaldo", "HDP")

# Adicionamos a barra final para que o R entenda que é um diretório
# BASE_PATH <- paste0(BASE_PATH, "/") # Embora não seja estritamente necessário com paste0/file.path

# --- 2. Verificação (Muito Importante!) ---
# Listar os arquivos para confirmar se o caminho está correto
print("Verificando os arquivos no caminho:")
print(list.files(BASE_PATH))

# --- 3. Carregar os arquivos CSV gerados pelo Python ---
print("\n--- Carregando arquivos do HDP no R ---")

# 2.1. Carregar o dataframe de músicas com a atribuição de tópicos
df_musicas_topicos <- read.csv(
  file = file.path(BASE_PATH, "musicas_com_topico.csv"),
  stringsAsFactors = FALSE
)

# 2.2. Carregar o resumo dos tópicos
df_resumo_topicos_hdp <- read.csv(
  file = file.path(BASE_PATH, "resumo_topicos_hdp.csv"),
  stringsAsFactors = FALSE
)

# 2.3. Carregar a tabela de artistas por tópico
df_artistas_por_topico_hdp <- read.csv(
  file = file.path(BASE_PATH, "artistas_por_topico_hdp.csv"),
  stringsAsFactors = FALSE
)

# --- 3. Inspecionar os dataframes carregados ---
print("\nDataFrame de Músicas e Tópicos (df_musicas_topicos):")
View(df_musicas_topicos)
print(dim(df_musicas_topicos))
print(colnames(df_musicas_topicos))

print("\nDataFrame de Resumo de Tópicos (df_resumo_topicos_hdp):")
View(df_resumo_topicos_hdp)
print(dim(df_resumo_topicos_hdp))
print(colnames(df_resumo_topicos_hdp))

print("\nDataFrame de Artistas por Tópico (df_artistas_por_topico_hdp):")
View(df_artistas_por_topico_hdp)
print(dim(df_artistas_por_topico_hdp))
print(colnames(df_artistas_por_topico_hdp))

print("\nCarregamento de dados concluído no R.")
#================================================================================

# Certifique-se de que os pacotes necessários estão carregados
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(plotly)
library(htmlwidgets)

# --- 1. Carregar os Dados do HDP (Assumimos que já foram carregados no script anterior) ---
# ... (os dataframes df_musicas_topicos, df_resumo_topicos_hdp e df_artistas_por_topico_hdp já existem na memória)

# --- 2. Identificar os 10 tópicos HDP com mais músicas ---
top_10_topicos_hdp_por_musicas <- df_resumo_topicos_hdp %>%
  arrange(desc(num_musicas)) %>%
  head(10) %>%
  pull(topico_hdp)

cat(paste0("--- Os 10 tópicos HDP com mais músicas são: ", paste(top_10_topicos_hdp_por_musicas, collapse = ", "), " ---\n"))


# --- 3. Visualizar a Tabela Alinhada de Artistas (gerada no Python) ---
cat("\n\n=============================================\n")
cat(" ARTISTAS MAIS FREQUENTES NOS 10 TÓPICOS HDP COM MAIS MÚSICAS\n")
cat(" (FILTRADA DA TABELA JÁ GERADA NO PYTHON)\n")
cat("=============================================\n")

# A CORREÇÃO ESTÁ AQUI: Gerar os nomes de coluna com o ponto (.) para corresponder ao que o R leu
nomes_colunas_topicos_esperados <- paste0("Tópico.", top_10_topicos_hdp_por_musicas)
nomes_colunas_topicos_esperados_com_posicao <- c("posicao", nomes_colunas_topicos_esperados)

tabela_artistas_hdp_alinhada_top_musicas <- df_artistas_por_topico_hdp %>%
  select(all_of(nomes_colunas_topicos_esperados_com_posicao)) %>%
  mutate_all(~replace_na(., "")) # Preenche NAs com strings vazias para uma exibição mais limpa

View(tabela_artistas_hdp_alinhada_top_musicas)


# --- 4. Preparar os dados para os gráficos de proporção por ano ---

# Filtrar o DataFrame principal para incluir apenas os dados desses 10 tópicos
dados_filtrados_para_grafico <- df_musicas_topicos %>%
  filter(topico_hdp %in% top_10_topicos_hdp_por_musicas)

# Calcular o total de músicas por ano (em relação ao dataset COMPLETO para proporção correta)
total_musicas_por_ano_completo <- df_musicas_topicos %>%
  group_by(Ano) %>%
  summarise(total_musicas = n())

# Calcular o número de músicas por tópico por ano, apenas para os 10 tópicos filtrados
musicas_topico_ano_filtrado <- dados_filtrados_para_grafico %>%
  group_by(Ano, topico_hdp) %>%
  summarise(contagem_topico_ano = n(), .groups = "drop")

# Juntar para calcular a proporção (contagem do tópico filtrado / total de músicas no ano)
dados_grafico_proporcao_filtrado <- musicas_topico_ano_filtrado %>%
  left_join(total_musicas_por_ano_completo, by = "Ano") %>%
  mutate(proporcao = contagem_topico_ano / total_musicas)


# --- 5. Gerar o gráfico de linhas estático e interativo ---

cat("\n\n=============================================\n")
cat(" GRÁFICO: PROPORÇÃO DOS 10 TÓPICOS HDP MAIS FREQUENTES POR ANO\n")
cat("=============================================\n")

# Gráfico estático ggplot2
p_estatico <- ggplot(dados_grafico_proporcao_filtrado,
                     aes(x = Ano, y = proporcao, color = factor(topico_hdp))) +
  geom_line(linewidth = 1) +
  labs(
    title = "Proporção dos 10 Tópicos HDP Mais Frequentes ao Longo dos Anos",
    x = "Ano",
    y = "Proporção de Músicas",
    color = "Tópico HDP"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

print(p_estatico)

# Gráfico interativo plotly
p_interativo <- ggplot(dados_grafico_proporcao_filtrado,
                       aes(x = Ano, y = proporcao, color = factor(topico_hdp),
                           text = paste("Tópico: ", topico_hdp,
                                        "<br>Ano: ", Ano,
                                        "<br>Proporção: ", scales::percent(proporcao)))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Proporção dos 10 Tópicos HDP Mais Frequentes ao Longo dos Anos",
    x = "Ano",
    y = "Proporção de Músicas",
    color = "Tópico HDP"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

plotly_hdp_top10_proporcao_final <- ggplotly(p_interativo, tooltip = "text")

print(plotly_hdp_top10_proporcao_final)

# Opcional: Salvar o gráfico interativo
caminho_arquivo_html <- file.path(BASE_PATH, "grafico_hdp_top10_proporcao_interativo.html")
saveWidget(plotly_hdp_top10_proporcao_final, file = caminho_arquivo_html, selfcontained = TRUE)
cat(paste0("\nGráfico interativo salvo como: '", caminho_arquivo_html, "'\n"))