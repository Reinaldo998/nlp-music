library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales) # Para scales::percent

# --- 1. PREPARAÇÃO DE DADOS (Cálculo da Proporção) ---

# 1.1. Calcular o total de músicas por ano na base COMPLETA (Denominador)
# Assumimos que 'data_dp' está na memória.
total_musicas_por_ano_completo <- data_dp %>%
  group_by(Ano) %>%
  summarise(total_musicas = n(), .groups = "drop")

# 1.2. Filtrar o DataFrame principal para incluir apenas os 10 tópicos de interesse
# Assumimos que 'top_10_topicos_por_musicas' está na memória e ordenado.
dados_filtrados_para_grafico <- data_dp %>%
  filter(Grupos_MM_DP %in% top_10_topicos_por_musicas)

# 1.3. Contar as músicas por Tópico e por Ano (Numerador)
musicas_topico_ano_dp_filtrado <- dados_filtrados_para_grafico %>%
  group_by(Ano, Grupos_MM_DP) %>%
  summarise(contagem_topico_ano = n(), .groups = "drop")

# 1.4. Juntar e Calcular a Proporção
dados_grafico_proporcao_dp <- musicas_topico_ano_dp_filtrado %>%
  left_join(total_musicas_por_ano_completo, by = "Ano") %>%
  mutate(proporcao = contagem_topico_ano / total_musicas)


# --- 2. CRIAÇÃO DO MAPA DE PARES (Agrupamento 2 a 2) ---

top_10_ids <- top_10_topicos_por_musicas
mapa_pares <- tibble(
  Grupos_MM_DP = top_10_ids,
  ordem_global = seq_along(top_10_ids), # Usando seq_along para índice sequencial
  # Cria o rótulo do par: (1, 2) -> Par 1; (3, 4) -> Par 2
  rotulo_par = paste("Par", ceiling(ordem_global / 2))
)

# Juntar o mapeamento ao dataframe de proporção
dados_grafico_proporcao_dp_pares <- dados_grafico_proporcao_dp %>%
  left_join(mapa_pares %>% select(Grupos_MM_DP, rotulo_par), by = "Grupos_MM_DP")

# Obter os nomes dos pares (Par 1, Par 2, Par 3, Par 4, Par 5)
pares_unicos <- unique(dados_grafico_proporcao_dp_pares$rotulo_par)


# --- 3. LOOP PARA GERAR E IMPRIMIR 5 GRÁFICOS INDIVIDUAIS ---

cat("\n\n=============================================\n")
cat(" GERANDO 5 PLOTS INDIVIDUAIS (2 TÓPICOS POR IMAGEM)\n")
cat("=============================================\n")

# Cria uma lista para armazenar os plots se você quiser manipulá-los depois
lista_plots_finais <- list()

for (i in seq_along(pares_unicos)) {
  par_atual <- pares_unicos[i]
  
  # 3.1. Filtrar o dataframe para o par atual (ex: "Par 1")
  df_plot_individual <- dados_grafico_proporcao_dp_pares %>%
    filter(rotulo_par == par_atual)
  
  # 3.2. Identificar os tópicos neste par para o título e legenda
  topicos_neste_par <- unique(df_plot_individual$Grupos_MM_DP)
  titulo_topicos <- paste(topicos_neste_par, collapse = " e ")
  
  # 3.3. Gerar o gráfico
  p_individual <- ggplot(df_plot_individual,
                         aes(x = Ano, y = proporcao,
                             color = factor(Grupos_MM_DP), # Cor pelo ID do tópico
                             group = factor(Grupos_MM_DP))) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = paste0("Proporção dos Tópicos ", titulo_topicos, " ao Longo dos Anos"),
      x = "Ano",
      y = "Proporção de Músicas",
      color = "Tópico"
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) + # Melhorar a quebra do eixo X
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "right",
      legend.title = element_text(face = "bold")
    )
  
  # 3.4. Imprimir e Salvar
  print(p_individual)
  
  # Adicionar o plot à lista para referência futura
  lista_plots_finais[[paste0("plot_", par_atual)]] <- p_individual
  
  # Exemplo de como salvar para a dissertação (descomente e ajuste o caminho)
  # ggsave(
  #     filename = paste0("Proporcao_Topicos_", gsub(" ", "_", par_atual), ".tiff"),
  #     plot = p_individual,
  #     width = 8, 
  #     height = 6,
  #     units = "in",
  #     dpi = 300 # Alta resolução para dissertação
  # )
}

cat("\nExecução concluída. Cinco gráficos individuais foram impressos no RStudio.\n")
cat("\nPara salvar em alta resolução (como TIFF ou PDF) para a dissertação, use a função 'ggsave'.\n")